

KNNmc <- function(train,test,label_class = NULL,seed = 0,NeighborNumber = 1,Neighborhood = "INRANGE",DistanceType = "EUCLIDEAN",useRMI = "YES",monotonicRMILevel = 0.1){
  alg <- KNNmcR6$new()
  alg$setParameters(train,test,label_class,seed,NeighborNumber,Neighborhood,DistanceType,useRMI,monotonicRMILevel)
  # alg$run()
  # return (alg$get_measures())
}

###OBJETO
KNNmcR6 <- R6::R6Class("KNNmc",
    inherit = monotonicAlgorithm,
    public = list(

      name = "KNNmc",
      jar = "KNNmc.jar",

      #Read parameters necessary
      setParameters = function(train,test,label_class,seed,NeighborNumber,Neighborhood,DistanceType,useRMI,monotonicRMILevel){

        if(is.null(label_class)){
          private$remove_files_folder()
          stop(" Label_class cannot be NULL",call. = FALSE)

        }else{

          private$generateDir(self$name)

          #Create dataset train keel
          path_train <- private$create_dataset(train,"dataset_monotonic-tra.dat",label_class,file.path(private$filesPath,self$name))

          #Create dataset test keel
          path_test <- private$create_dataset(test,"dataset_monotonic-test.dat",label_class,file.path(private$filesPath,self$name))

          #Create config
          private$create_config(path_train,path_test,file.path(private$filesPath,self$name),self$name)


          if(!is.null(self$jar)){
            private$insert_attributes(seed,NeighborNumber,Neighborhood,DistanceType,useRMI,monotonicRMILevel)
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

      insert_attributes = function(seed,NeighborNumber,Neighborhood,DistanceType,useRMI,monotonicRMILevel){

        name_file <- file.path(private$filesPath,self$name,private$configName)

        if(seed == 0){
          seed <- as.integer(runif(1, min=1000000000, max=9999999999))
        }
        write(paste("\nseed = ",as.character(seed),sep=""),file=name_file,append = TRUE)


        write(paste("NeighborNumber = ",as.character(NeighborNumber),sep=""),file=name_file,append = TRUE)

        neigh <- dtype <- private$check_neighbourhood(Neighborhood)
        write(paste("Neighborhood = ",neigh,sep=""),file=name_file,append = TRUE)

        dtype <- private$check_distance_type(DistanceType)
        write(paste("DistanceType = ",as.character(dtype),sep=""),file=name_file,append = TRUE)

        useRMI <- private$check_userRMI(useRMI)
        write(paste("useRMI = ",as.character(useRMI),sep=""),file=name_file,append = TRUE)

        if(monotonicRMILevel > 1) porcentage <- 1
        if(monotonicRMILevel < 0) porcentage <- 0
        write(paste("monotonicRMILevel = ",as.character(monotonicRMILevel),sep=""),file=name_file,append = TRUE)

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
                 stop("Distance type no valid. The options are: EUCLIDEAN,MANHATTAN,HVDM\n",call. = FALSE)
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
                 neighbourhood <- "OUTINRANGE"
               },
               {
                 private$remove_files_folder(file.path(private$filesPath,self$name))
                 stop("Neighbourhood no valid. The options are: INRANGE, OUTRANGE\n",call. = FALSE)
               }
        )
        return(neighbourhood)
      },

      check_userRMI = function(name){

        useRMI <- NULL
        switch(name,
               YES ={
                 useRMI <- "YES"
               },
               NO ={
                 useRMI <- "NO"
               },
               {
                 private$remove_files_folder(file.path(private$filesPath,self$name))
                 stop("useRMI no valid. The options are: YES or NO\n",call. = FALSE)
               }
        )
        return(useRMI)
      }
    )
)
