

OSDL <- function(train,test,label_class = NULL,seed = 0,ClassificationType = "MED",Balanced = "no",Weighted = "no",
                 TuneInterpolationParameter = "no",InterpolationParameter = 0.5,LowerBound = 0,UpperBound = 1,
                 InterpolationParameterStepSize = 10,useRMI = "yes"){
  alg <- OSDLR6$new()
  alg$setParameters(train,test,label_class,seed,ClassificationType,Balanced,Weighted,TuneInterpolationParameter,
                    InterpolationParameter,LowerBound,UpperBound,InterpolationParameterStepSize,useRMI)
  alg$run()
  return(alg$get_measures())
}

###OBJETO
OSDLR6 <- R6::R6Class("OSDL",
    inherit = monotonicAlgorithm,
    public = list(

      name = "OSDL",
      jar = "OSDL.jar",

      #Read parameters necessary
      setParameters = function(train,test,label_class,seed,ClassificationType,Balanced,Weighted,TuneInterpolationParameter,
                               InterpolationParameter,LowerBound,UpperBound,InterpolationParameterStepSize,useRMI){

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
            private$insert_attributes(seed,ClassificationType,Balanced,Weighted,TuneInterpolationParameter,
                                      InterpolationParameter,LowerBound,UpperBound,InterpolationParameterStepSize,
                                      useRMI)
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

      insert_attributes = function(seed,ClassificationType,Balanced,Weighted,TuneInterpolationParameter,
                                   InterpolationParameter,LowerBound,UpperBound,InterpolationParameterStepSize,
                                   useRMI){

        name_file <- file.path(private$filesPath,self$name,private$configName)

        if(seed == 0){
          seed <- runif(1, min=0, max=100000)
          seed <- signif(seed, digits = 8)

        }
        write(paste("\nseed = ",seed,sep=""),file=name_file,append = TRUE)

        write("Debug = no",file=name_file,append = TRUE)

        class_type <- private$check_classification_type(ClassificationType)
        write(paste("ModeResolution = ",as.character(class_type),sep=""),file=name_file,append = TRUE)

        balanced <- private$check_yes_no("Balanced",Balanced)
        write(paste("Balanced = ",as.character(balanced),sep=""),file=name_file,append = TRUE)

        w <- private$check_yes_no("Weighted",Weighted)
        write(paste("Weighted = ",as.character(w),sep=""),file=name_file,append = TRUE)

        t_interpolation <- private$check_yes_no("TuneInterpolationParameter",TuneInterpolationParameter)
        write(paste("TuneInterpolationParameter = ",as.character(t_interpolation),sep=""),file=name_file,append = TRUE)

        write(paste("InterpolationParameter = ",as.character(InterpolationParameter),sep=""),file=name_file,append = TRUE)

        write(paste("LowerBound = ",as.character(LowerBound),sep=""),file=name_file,append = TRUE)

        write(paste("UpperBound = ",as.character(UpperBound),sep=""),file=name_file,append = TRUE)

        write(paste("InterpolationParameterStepSize = ",as.character(InterpolationParameterStepSize),sep=""),file=name_file,append = TRUE)

        useRMI <- private$check_yes_no("userRMI",useRMI)
        write(paste("useRMI = ",as.character(useRMI),sep=""),file=name_file,append = TRUE)

      },
      check_classification_type = function(name){

        class_type <- NULL
        switch(name,
               MED ={
                 class_type <- "MED"
               },
               RMED ={
                 class_type <- "RMED"
               },
               MAX ={
                 class_type <- "MAX"
               },
               WSUM ={
                 class_type <- "WSUM"
               },
               REG ={
                 class_type <- "REG"
               },
               {
                 private$remove_files_folder(file.path(private$filesPath,self$name))
                 stop("Classification Type no valid. The options are: MED,RMED,MAX,WSUM and REG\n",call. = FALSE)
               }
        )
        return(class_type)
      },

      check_yes_no = function(name,value){

        out <- NULL
        switch(value,
               yes ={
                 out <- "yes"
               },
               no ={
                 out <- "no"
               },
               {
                 private$remove_files_folder(file.path(private$filesPath,self$name))
                 stop(paste0(name," no valid. The options are: YES or NO\n"),call. = FALSE)
               }
        )
        return(out)
      }
    )
)
