

KNNmc <- function(train,test,label_class = NULL,seed = NULL,NeighborNumber = 1,Neighborhood = "INRANGE",DistanceType = "EUCLIDEAN"){
  alg <- KNNmcR6$new()
  alg$setParameters(train,test,label_class,seed,NeighborNumber,Neighborhood,DistanceType)
  alg$run()
  return(alg$get_measures())
}

###OBJETO
KNNmcR6 <- R6::R6Class("KNNmc",
    inherit = monotonicAlgorithm,
    public = list(

      name = "KNNmc",
      jar = "KNNmc.jar",

      #Read parameters necessary
      setParameters = function(train,test,label_class,seed,NeighborNumber,Neighborhood,DistanceType){

        if(is.null(label_class) || is.null(seed)){
          private$remove_files_folder(file.path(private$filesPath,self$name))
          stop(" Label_class or seed cannot be NULL",call. = FALSE)

        }else{

          private$generateDir(self$name)

          #Create dataset train keel
          path_train <- private$create_dataset(train,"dataset_monotonic-tra.dat",label_class,file.path(private$filesPath,self$name))

          #Create dataset test keel
          path_test <- private$create_dataset(test,"dataset_monotonic-test.dat",label_class,file.path(private$filesPath,self$name))

          #Create config
          private$create_config(path_train,path_test,file.path(private$filesPath,self$name),self$name)


          if(!is.null(self$jar)){
            private$insert_attributes(seed,NeighborNumber,Neighborhood,DistanceType)
          }

        }

      },

      #Execute algorithm
      run =  function(){

        if(!is.null(self$jar)){

          #Download all jar from github
          private$download_jar(self$name,paste0(self$name,".zip"))

          #Execute algorithm
          private$execute(file.path(private$downloadPath,self$name,self$jar),file.path(private$filesPath,self$name,private$configName))

        }else{
          private$remove_files_folder(file.path(private$filesPath,self$name))
          stop("You need to call setParameters function",call. = FALSE)
        }
      },

      get_measures = function(){
        output_json <- file.path(private$filesPath,self$name,"result0e0.json")
        private$measures(output_json)
      }

    ),
    private = list(

      insert_attributes = function(seed,NeighborNumber,Neighborhood,DistanceType){

        name_file <- file.path(private$filesPath,self$name,private$configName)

        if(seed == 0){
          seed <- runif(1, min=0, max=100000)
          seed <- signif(seed, digits = 8)

        }
        write(paste("\nseed = ",seed,sep=""),file=name_file,append = TRUE)

        write(paste("NeighborNumber = ",as.character(NeighborNumber),sep=""),file=name_file,append = TRUE)

        neigh <- private$check_neighbourhood(Neighborhood)
        write(paste("Neighborhood = ",neigh,sep=""),file=name_file,append = TRUE)

        dtype <- private$check_distance_type(DistanceType)
        write(paste("DistanceType = ",as.character(dtype),sep=""),file=name_file,append = TRUE)

        write("useRMI = NO",file=name_file,append = TRUE)

        write("monotonicRMILevel = 0.1",file=name_file,append = TRUE)

      },

      check_distance_type = function(name){

        dtype <- NULL
        switch(name,
               EUCLIDEAN={
                 dtype <- "EUCLIDEAN"
               },
               MANHATTAN={
                 dtype <- "MANHATTAN"
               },
               HVDM={
                 dtype <- "HVDM"
               },
               {
                 private$remove_files_folder(file.path(private$filesPath,self$name))
                 stop("Distance type no valid. The options are: EUCLIDEAN,MANHATTAN and HVDM\n",call. = FALSE)
               }
        )
        return(dtype)
      },

      check_neighbourhood = function(name){

        neighbourhood <- NULL
        switch(name,
               INRANGE ={
                 neighbourhood <- "INRANGE"
               },
               OUTINRANGE ={
                 neighbourhood <- "OUTRANGE"
               },
               {
                 private$remove_files_folder(file.path(private$filesPath,self$name))
                 stop("Neighbourhood no valid. The options are: INRANGE and OUTRANGE\n",call. = FALSE)
               }
        )
        return(neighbourhood)
      }
    )
)
