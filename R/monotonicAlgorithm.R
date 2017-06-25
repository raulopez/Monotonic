monotonicAlgorithm <- R6::R6Class("monotonicAlgorithm",
   public = list(
     #Initialize
     initialize = function(){
       #Librerias y archivos
       if (!requireNamespace("jsonlite", quietly = TRUE)) {
         stop("jsonlite package needed for this function to work. Please install it.",call. = FALSE)
       }
       if (!requireNamespace("downloader", quietly = TRUE)) {
         stop("downloader package needed for this function to work. Please install it.",call. = FALSE)
       }

<<<<<<< HEAD
     }

=======
       # 1 Create folder structure necessary for save file
       private$generateDir()

     },

     #Execute algorithm
     run =  function(){
       stop("This function will be implemeted for each algotithm",call. = FALSE)
     },

     #measures function
     measures = function(){

       if(file.exists(private$jsonPath)){
         mydf <- jsonlite::fromJSON(private$jsonPath)
         dim(mydf$matrix_test) <- c(dim(mydf$matrix_test)[1],dim(mydf$matrix_test)[1])
         dim(mydf$matrix_train) <- c(dim(mydf$matrix_train)[1],dim(mydf$matrix_train)[1])
         mydf$decision_tree <- NULL
       }else{
         stop("Cannot read json file\n",call. = FALSE)
       }
       return(mydf)
     }
>>>>>>> 256c36ff291634eb269924dc0d1d583debeed7ae
),
 private = list(

     #jar file
     configName = "config0.txt",

     #downloadPath
     downloadPath = file.path(system.file(package = "Monotonic"), "download"),

<<<<<<< HEAD
     #exes path
=======
     #files path
>>>>>>> 256c36ff291634eb269924dc0d1d583debeed7ae
     filesPath = file.path(system.file(package = "Monotonic"), "files"),

     #dataset path
     datasetPath = file.path(system.file(package = "Monotonic"), "dataset"),

<<<<<<< HEAD
     githubPath = "https://github.com/raulopez/Monotonic/raw/master/jar",

     #Private function
     #Download Jar
     download_jar = function(zip_folder_name,name_zip){

       url_path <- file.path(private$githubPath,zip_folder_name,name_zip)
       path <- file.path(private$downloadPath,zip_folder_name)
       zip_path <- file.path(private$downloadPath,zip_folder_name,name_zip)

       downloader::download(url=url_path ,destfile = zip_path, mode ="wb", quiet = TRUE)
       unzip(zipfile = zip_path, exdir = path)
     },

     #Create dataset files function
     create_dataset = function(dataset,name,label_class,folder_name){

       name_file <- file.path(folder_name,name)
=======
     #json path
     jsonPath = file.path(system.file(package = "Monotonic"), "json","result0e0.json"),

     #Private function
     #Download Jar
     download_jar = function(path_file){

       path <- file.path(path_file,"monotonicTree.zip")
       downloader::download(url="https://github.com/raulopez/Monotonic/blob/master/jar/monotonicTree/monotonicTree.zip",destfile = path, mode ="wb")
       cat("path: ",path,"downloadpath: ",private$downloadPath,"\n")
       # unzip(zipfile = path, exdir = private$downloadPath)
     },

     #Create dataset files function
     create_dataset = function(dataset,name,label_class){

       name_file <- file.path(private$filesPath,name)
>>>>>>> 256c36ff291634eb269924dc0d1d583debeed7ae

       output <- NULL
       input <- NULL
       string <- "@relation dataset monotonic"
       write(string,file=name_file,append = TRUE)

       for(i in 1:length(names(dataset))){

         if(names(dataset)[i]!=label_class){
           input <- c(input,names(dataset)[i])
         }
         else{
           output <- names(dataset)[i]
         }
         line <- NULL

         if(names(dataset)[i] == label_class && (class(dataset[1,i])=="numeric" || class(dataset[1,i])=="integer")){
           head <- paste("@attribute",names(dataset)[i],sep=" ")
           level <- paste(seq(min(dataset[,i]),max(dataset[,i])),collapse = ",")
           interval <- paste("{",level,"}",sep = "")
           line <- paste(head,interval,sep =" ")
         }

         else if(class(dataset[1,i])=="numeric"){

           head <- paste("@attribute",names(dataset)[i],"real",sep=" ")
           interval <- paste("[",min(dataset[,i]),", ",max(dataset[,i]),"]",sep = "")
           line <- paste(head,interval,sep =" ")

         }else if(class(dataset[1,i])=="integer"){

           head <- paste("@attribute",names(dataset)[i],"integer",sep=" ")
           interval <- paste("[",min(dataset[,i]),", ",max(dataset[,i]),"]",sep = "")
           line <- paste(head,interval,sep =" ")

         }else if(class(dataset[1,i])=="factor"){
           head <- paste("@attribute",names(dataset)[i],sep=" ")
           level <- paste(levels(dataset[,5]),collapse=", ")
           interval <- paste("{",level,"}",sep = "")
           line <- paste(head,interval,sep =" ")
         }

         if(!is.null(line)){
           write(line,file=name_file,append = TRUE)
         }
       }

       label_input <- paste(input,collapse=", ")
       line <- paste("@inputs ",label_input ,sep = "")
       write(line,file=name_file,append = TRUE)
       line <- paste("@outputs ",output ,sep = "")
       write(line,file=name_file,append = TRUE)
       write("@data",file=name_file,append = TRUE)
       write.table(dataset, file = name_file, append = TRUE, quote = FALSE, sep = ", ",eol = "\n", na = "NA", dec = ".", row.names = FALSE,col.names = FALSE)

       return(name_file)
     },

     #Create config
<<<<<<< HEAD
     create_config = function(train,test,folder_name,name_algorithm){


       name_file <- file.path(folder_name,private$configName)

       string <- paste0("algorithm = ",name_algorithm)
=======
     create_config = function(train,test){


       name_file <- paste0(private$filesPath,"/",private$configName)

       string <- "algorithm = Monotonic Induction Decision"
>>>>>>> 256c36ff291634eb269924dc0d1d583debeed7ae
       write(string,file=name_file,append = TRUE)
       input <- paste("inputData =",shQuote(train),shQuote(test),shQuote(test),sep=' ')
       write(input,file=name_file,append = TRUE)

<<<<<<< HEAD
       output_train <- file.path(folder_name,"result0.tra")
       output_test <- file.path(folder_name,"result0.tst")
       output_resultado <- file.path(folder_name,"result0e0.txt")
       output_json <- file.path(folder_name,"result0e0.json")
=======
       output_train <- file.path(system.file(package = "monotonicTree"),"files","result0.tra")
       output_test <- file.path(system.file(package = "monotonicTree"),"files","result0.tst")
       output_resultado <- file.path(system.file(package = "monotonicTree"),"files","result0e0.txt")
       output_json <- file.path(system.file(package = "monotonicTree"),"result0e0.json")
>>>>>>> 256c36ff291634eb269924dc0d1d583debeed7ae

       output <- paste("outputData =",shQuote(output_train),shQuote(output_test),shQuote(output_resultado),shQuote(output_json),sep=' ')
       write(output,file=name_file,append = TRUE)
     },

     #Generate experiment directory
<<<<<<< HEAD
     generateDir = function(name){

       dir.create(private$downloadPath,showWarnings = FALSE)
       dir.create(private$filesPath,showWarnings = FALSE)
       dir.create(private$datasetPath,showWarnings = FALSE)

       path_d <- file.path(private$downloadPath,name)
       path_f <- file.path(private$filesPath,name)

       unlink(list.files(path_f,full.names = TRUE), recursive = TRUE, force = TRUE)
       unlink(list.files(path_d,full.names = TRUE), recursive = TRUE, force = TRUE)

       dir.create(path_f,showWarnings = FALSE)
       dir.create(path_d,showWarnings = FALSE)
=======
     generateDir = function(){
       dir.create(private$downloadPath,showWarnings = FALSE)
       dir.create(private$filesPath,showWarnings = FALSE)
       dir.create(private$datasetPath,showWarnings = FALSE)
>>>>>>> 256c36ff291634eb269924dc0d1d583debeed7ae
     },

     #Execute experiment
     execute = function(jar,config){
<<<<<<< HEAD

       execute <- paste("java -jar",jar,config,sep=" ")
       system(execute,intern = FALSE,show.output.on.console = FALSE)
=======
       execute <- paste("java -jar",jar,config,sep=" ")
       system(execute,intern = TRUE,show.output.on.console = FALSE)
>>>>>>> 256c36ff291634eb269924dc0d1d583debeed7ae

     },

     #Remove all files in folder files
<<<<<<< HEAD
     remove_files_folder = function(path){
       if(length(list.files(path)) > 0){
         unlink(list.files(path,full.names = TRUE), recursive = TRUE, force = TRUE)
       }
     },

     #measures function
     measures = function(path){

       mydf <- jsonlite::fromJSON(path)
       dim(mydf$matrix_test) <- c(dim(mydf$matrix_test)[1],dim(mydf$matrix_test)[1])
       dim(mydf$matrix_train) <- c(dim(mydf$matrix_train)[1],dim(mydf$matrix_train)[1])
       return(mydf)
     }
=======
     remove_files_folder = function(){
       if(length(list.files(private$filesPath)) > 0){
         unlink(list.files(private$filesPath,full.names = TRUE), recursive = TRUE, force = TRUE)
       }
     }

>>>>>>> 256c36ff291634eb269924dc0d1d583debeed7ae
   )
)
