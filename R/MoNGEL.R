

MoNGEL <- function(train,test,label_class = NULL, seed){
  alg <- MoNGELR6$new()
  alg$setParameters(train,test,label_class,seed)
  # alg$run()
  # return(alg$get_measures())
}

###OBJETO
MoNGELR6 <- R6::R6Class("MoNGEL",
    inherit = monotonicAlgorithm,
    public = list(

      name = "MoNGEL",
      jar = "MoNGEL.jar",

      #Read parameters necessary
      setParameters = function(train,test,label_class,seed){

        if(is.null(label_class)){
          private$remove_files_folder(file.path(private$filesPath,self$name))
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
            private$insert_attributes(seed)
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

      insert_attributes = function(seed){

        name_file <- file.path(private$filesPath,self$name,private$configName)

        if(seed == 0){
          seed <- runif(1, min=0, max=100000)
          seed <- signif(seed, digits = 8)

        }
        write(paste("\nseed = ",seed,sep=""),file=name_file,append = TRUE)


      }
    )
)
