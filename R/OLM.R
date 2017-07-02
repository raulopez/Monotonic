

OLM <- function(train,test,label_class = NULL,seed = 0,ModeResolution = "Conservative",ModeClassification = "Conservative",useRMI = "YES"){
  alg <- OLMR6$new()
  alg$setParameters(train,test,label_class,seed,ModeResolution,ModeClassification,useRMI)
  alg$run()
  return(alg$get_measures())
}

###OBJETO
OLMR6 <- R6::R6Class("OLM",
    inherit = monotonicAlgorithm,
    public = list(

      name = "OLM",
      jar = "OLM.jar",

      #Read parameters necessary
      setParameters = function(train,test,label_class,seed,ModeResolution,ModeClassification,useRMI){

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
            private$insert_attributes(seed,ModeResolution,ModeClassification,useRMI)
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

      insert_attributes = function(seed,ModeResolution,ModeClassification,useRMI){

        name_file <- file.path(private$filesPath,self$name,private$configName)

        if(seed == 0){
          seed <- runif(1, min=0, max=100000)
          seed <- signif(seed, digits = 8)

        }
        write(paste("\nseed = ",seed,sep=""),file=name_file,append = TRUE)

        m_resolution <- private$check_mode_resolutions(ModeResolution)
        write(paste("ModeResolution = ",as.character(m_resolution),sep=""),file=name_file,append = TRUE)

        m_class <- private$check_mode_classification(ModeClassification)
        write(paste("ModeClassification = ",as.character(m_class),sep=""),file=name_file,append = TRUE)

        useRMI <- private$check_userRMI(useRMI)
        write(paste("useRMI = ",as.character(useRMI),sep=""),file=name_file,append = TRUE)

      },

      check_mode_classification = function(name){

        m_class <- NULL
        switch(name,
               Conservative={
                 m_class <- "Conservative"
               },
               MANHATTAN={
                 m_class <- "MANHATTAN"
               },
               {
                 private$remove_files_folder(file.path(private$filesPath,self$name))
                 stop("Mode classification no valid. The options are: Conservative and MANHATTAN\n",call. = FALSE)
               }
        )
        return(m_class)
      },

      check_mode_resolutions = function(name){

        m_resolutions <- NULL
        switch(name,
               Conservative ={
                 m_resolutions <- "Conservative"
               },
               Radom ={
                 m_resolutions <- "Radom"
               },
               Average ={
                 m_resolutions <- "Average"
               },
               {
                 private$remove_files_folder(file.path(private$filesPath,self$name))
                 stop("Mode Resolution no valid. The options are: Conservative, Random and Average\n",call. = FALSE)
               }
        )
        return(m_resolutions)
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
