

monotonicTree <- function(train,test,label_class = NULL,method = "NULL",pruned = TRUE,confidence = 0.25,importance = 10,itemsetsPerLeaf = 2,porcentage = 0.8){
  alg <- monotonicTreeR6$new()
  alg$setParameters(train,test,label_class,method,pruned,confidence,importance,itemsetsPerLeaf,porcentage)
  alg$run()
  return (alg$get_measures())
}

###OBJETO
monotonicTreeR6 <- R6::R6Class("monotonicTree",
    inherit = monotonicAlgorithm,
    public = list(

      name = "monotonicTree",
      jar = NULL,

      #Read parameters necessary
      setParameters = function(train,test,label_class = NULL,method = "NULL",pruned = TRUE,confidence = 0.25,importance = 10,itemsetsPerLeaf = 2,porcentage = 0.8){

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
          private$create_config(path_train,path_test,file.path(private$filesPath,self$name),"Monotonic Induction Decision")

          #Jar file
          self$jar <- private$check_methods(method)

          if(!is.null(self$jar)){
            private$insert_attributes(pruned,confidence,importance,itemsetsPerLeaf,method,porcentage)
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

      insert_attributes = function(pruned,confidence,importance = 10,leaf,metric,porcentage){

        name_file <- file.path(private$filesPath,self$name,private$configName)

          if(class(pruned) != "logical"){
            private$remove_files_folder()
            stop("PRUNED not logical value",call. = FALSE)

          }
          write(paste("\npruned = ",as.character(pruned),sep=""),file=name_file,append = TRUE)

          if(confidence > 1) confidence <- 1
          if(confidence < 0) confidence <- 0
          write(paste("confidence = ",as.character(confidence),sep=""),file=name_file,append = TRUE)

          if(metric == "MID"){
            write(paste("relative_importance_monotonicity =",as.character(importance),sep=""),file=name_file,append = TRUE)
          }

          write(paste("instancesPerLeaf = ",leaf,sep=""),file=name_file,append = TRUE)

          if(porcentage > 1) porcentage <- 1
          if(porcentage < 0) porcentage <- 0
          write(paste("porcentage_antimonotonic = ",as.character(porcentage),sep=""),file=name_file,append = TRUE)
      },

      check_methods = function(name){

        jar <- NULL
        switch(name,
           MID={
             jar <- "MID.jar"
           },
           RMI={
             jar <- "RMI.jar"
           },
           RSD={
             jar <- "RSD.jar"
             cat(jar)
           },
           RGD={
             jar <- "RGD.jar"
           },
           {
              private$remove_files_folder()
              stop("Metrics no valid. The options are: MID,RMI,RSD,RGD\n",call. = FALSE)
           }
        )
        return(jar)
      }
    )
)
