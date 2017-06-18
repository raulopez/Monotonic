###OBJETO
monotonicTree <- R6::R6Class("monotonicTree",
    inherit = monotonicAlgorithm,
    public = list(

      confidence = 0.25,
      itemsetsPerLeaf = 2,
      name = "monotonicTree",
      jar = NULL,
      path_monotonicTree = NULL,

      #Read parameters necessary
      setParameters = function(train,test,label_class = NULL,method = "NULL",pruned = TRUE,confidence = 0.25,importance = 10,itemsetsPerLeaf = 2,porcentage = 0.8){

        if(is.null(label_class)){
          private$remove_files_folder()
          stop(" Label_class cannot be NULL",call. = FALSE)

        }else{
          self$path_monotonicTree <- file.path(private$downloadPath,self$name)

          #Delete if exist files in folder files
          private$remove_files_folder()

          #Create dataset train keel
          path_train <- private$create_dataset(train,"dataset_monotonic-tra.dat",label_class)
          cat("Create dataset train\n")
          #Create dataset test keel
          path_test <- private$create_dataset(test,"dataset_monotonic-test.dat",label_class)
          cat("Create dataset test\n")

          #Create config
          private$create_config(path_train,path_test)
          cat("Create config\n")

          self$jar <- private$check_methods(method)
          if(!is.null(self$jar)){
            private$insert_attributes(pruned,confidence,importance,itemsetsPerLeaf,method,porcentage)
          }

        }

      },

      #Execute algorithm
      run =  function(){

        if(!is.null(self$path_monotonicTree)){

          #Create folder where download jar
          dir.create(self$path_monotonicTree,showWarnings = FALSE)

          #Download all jar from github
          private$download_jar(self$path_monotonicTree)

          #Execute algorithm
          # private$execute(self$path_monotonicTree,self$jar)
        }else{
          private$remove_files_folder()
          stop("You need to call setParameters function",call. = FALSE)
        }
      },

      # print monotonic tree
      printTree = function(){

        if(file.exists(private$jsonPath)){
          mydf <- jsonlite::fromJSON(private$jsonPath)
          o <- lapply(mydf$decision_tree,function(x){cat(x)})
        }else{
          stop("Cannot read decision tree from json file\n",call. = FALSE)
        }
      }
    ),
    private = list(

      insert_attributes = function(pruned,confidence,importance = 10,leaf,metric,porcentage){

        name_file <- paste0(private$filesPath,"/",private$configName)

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
