




# !!!!!!!!!!!!!!!! GENERIC SECTION BEGIN !!!!!!!!!!!!!!!!




if( !require( "tidyverse" ) ) { install.packages( "tidyverse" ) } ; library( tidyverse )
if( !require( "caret" ) ) { install.packages( "caret" ) } ; library( caret ) # for 'confusionMatrix'
if( !require( "lubridate" ) ) { install.packages( "lubridate" ) } ; library( lubridate ) # for the 'seconds_to_period' function
if( !require( "gridExtra" ) ) { install.packages( "gridExtra" ) } ; library( gridExtra ) # to arrange plots in grid

if( !require( "grDevices" ) ) { install.packages( "grDevices" ) } ; library( grDevices ) # to export images from plot (style transfer)
if( !require( "purrr" ) ) { install.packages( "purrr" ) } ; library( purrr )
if( !require( "R6" ) ) { install.packages( "R6" ) } ; library( R6 ) # to create the 'R6' EvaluatorClass class (style transfer)

library( grid ) # ?grid::rasterGrob # for 'rasterGrob' & to arrange plots in grid




## ########################################################
## Installing the 'rstudio/keras', 'tensorflow over GPU' ##
## and 'densenet' packages.                              ##
###########################################################
{
  # see list of compatible NVIDIA CUDA GPUs :
  # @see 'https://developer.nvidia.com/cuda-gpus'

  if( !require( "devtools" ) ) { install.packages( "devtools" ) } ; library( devtools )
  if( !require( "reticulate" ) ) {
    install_url( url = "https://cran.r-project.org/src/contrib/reticulate_1.12.tar.gz"
                 , type = "source" ) } ; library( reticulate ) # Interface to 'Python'
  if( !require( "tensorflow" ) ) {
    install_url( url = "https://cran.r-project.org/src/contrib/tensorflow_1.13.1.tar.gz"
                 , type = "source" ) } ; library( tensorflow ) # Interface to 'TensorFlow'
  if( !require( "keras" ) ) {
    install_version("keras", version = "2.1.6"
                    , repos = "http://cran.us.r-project.org")
    # Force an "archived" version to circumvent
    # the non-integration of Pull request #10023
    # (Refactor topological part of `engine` module)
    # from the original keras into "Rstudio/keras > 2.2" yet
    # (which causes densenet to fail)
  } ; library( keras )

  # A nice step-by-step guide :
  # How to Install TensorFlow with GPU Support on Windows 10
  # (Without Installing CUDA)
  # @see 'https://www.pugetsystems.com/labs/hpc/How-to-Install-TensorFlow-with-GPU-Support-on-Windows-10-Without-Installing-CUDA-UPDATED-1419/'
  #
  # Install Anaconda for Python 3.x
  # (https://www.anaconda.com/download/#windows)
  # before installing Keras.
  #
  # Visual Studio (required by CUDA on Windows) :
  # @see 'https://visualstudio.microsoft.com/downloads/'
  # To avoid any frustration when following the instalation steps,
  # be sure to read this short page :
  # @see 'https://www.tensorflow.org/install/gpu'
  # An other page of help (for manual install of tensorflow
  # when required, i.e. when version compatibility issues are encountered) :
  # @see 'https://www.tensorflow.org/install/pip'
  # NVIDIA CUDA Toolkit (might require a subsequent graphics driver update):
  # @see 'https://developer.nvidia.com/cuda-downloads?target_os=Windows&target_arch=x86_64&target_version=10&target_type=exelocal'
  # extract the cuDNN into the %PATH% :
  # @see 'https://developer.nvidia.com/rdp/cudnn-download'
  # if encountering the "no SSL" issue, run the below command on the Anaconda Powershell Prompt :
  # "conda install python=3.7.1=h33f27b4_4"
  # @see 'https://stackoverflow.com/questions/53541815/no-ssl-support-included-in-this-python-anaconda-python3-smtplib/53548593#53548593'

  # install_keras( tensorflow = "gpu" ) # UNCOMMENT AND RUN ONCE


  if( !require( densenet ) ) { devtools::install_github( "dfalbel/densenet" ) } ; library( densenet )
}
###########################################################




## #############################################
##         'amc_pdf_print' function           ##
################################################
# convenience method to print the pdf report   #
# with (optional) input parameters.            #
# In addition, allows to access                #
# 'global environment' variables from within   #
# the 'report printing' session (thus avoiding #
# to train a model and or reload r-packages    #
# each time).                                  #
################################################
amc_pdf_print <- function(
  paramsList = NULL
  , SumatraPDF_fullpath =
    "C:/PROGRA~1/RStudio/bin/sumatra/SumatraPDF.exe "
  , rootFolderPath = getwd()
) {
  t1 <- proc.time()
  
  outfile <- file.path( rootFolderPath, "Report.pdf" )
  
  rmarkdown::render(
    input = file.path( rootFolderPath, "/Report.Rmd" )
    , params = paramsList
    , encoding = "UTF-8"
    , output_file = outfile )
  print( ( proc.time() - t1 )[ "elapsed" ] ) # > 40''
  cmd = paste0(
    SumatraPDF_fullpath
    , "\"", outfile, "\"" )
  shell( cmd = cmd, intern = FALSE, wait = FALSE )
}
################################################




## #######################################################
## 'duration_string' function                           ##
##########################################################
# convenience method to custom-format duration strings   #
##########################################################
duration_string <- function(
  time_start
  , time_end = proc.time()
) {
  td <- as.POSIXlt( ( time_end - time_start )[ "elapsed" ]
                    , origin = lubridate::origin )
  round( second( td ) ) -> second( td )
  td <- seconds_to_period( td )
  return( tolower( td ) )
}
##########################################################




## #############################
##      'lsos' function       ##
################################
# convenience method to check  #
# on session RAM consumption   #
################################
# shorthand
lsos <- function(
  ...
  , n = 10
) {
  
  .ls.objects <- function (
    pos = 1
    , pattern
    , order.by
    , decreasing = FALSE
    , head = FALSE
    , n = 5
  ) {
    # list of loaded objects
    # @see 'https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session#4827843'
    napply <- function( names, fn ) sapply( names, function( x )
      fn( get( x, pos = pos ) ) )
    names <- ls( pos = pos, pattern = pattern )
    obj.class <- napply( names, function(x) as.character( class( x ) )[ 1 ] )
    obj.mode <- napply( names, mode )
    obj.type <- ifelse( is.na( obj.class ), obj.mode, obj.class )
    obj.prettysize <- napply( names, function( x ) {
      format( utils::object.size( x ), units = "auto" ) } )
    obj.size <- napply( names, object.size )
    obj.dim <- t( napply( names, function( x )
      as.numeric( dim( x ) )[ 1:4 ] ) )
    vec <- is.na( obj.dim )[ , 1 ] & ( obj.type != "function" )
    obj.dim[ vec, 1 ] <- napply( names, length )[ vec ]
    out <- data.frame( obj.type, obj.size, obj.prettysize, obj.dim )
    names(out) <- c( "Type", "Size", "PrettySize", "Length/Rows", "Columns", "Depth", "4thDim" )
    if ( !missing( order.by ) )
      out <- out[ order( out[[ order.by ]], decreasing = decreasing ), ]
    if ( head )
      out <- head( out, n )
    out
  }
  
  result_df <-
    .ls.objects( ...
                 , order.by = "Size"
                 , decreasing = TRUE
                 , head = TRUE
                 , n = n ) 
  print( result_df )
  rm( result_df, .ls.objects )
  cat( paste0( "Current session RAM consumption : "
               , format( object.size(
                 x = lapply( ls( envir = .GlobalEnv )
                             , get ) )
                 , units = "Gb" ) ) )
}
################################
# lsos()




# !!!!!!!!!!!!!!!!! GENERIC SECTION END !!!!!!!!!!!!!!!!!





# !!!!!!!!!!!!!!!! CIFAR-10 SECTION BEGIN !!!!!!!!!!!!!!!




## ###################################################
##          'amc_dataset_cifar10' function          ##
######################################################
# convenience method to load the 'cifar10' dataset   #
# into an in-memory R object equal in every aspect   #
# to the one returned by the'keras::dataset_cifar10' #
# function.                                          #
# (so that someone who doesn' want to install        #
# the 'rstudio/keras' package can still navigate     #
# that dataset in R)                                 #
######################################################
amc_dataset_cifar10 <- function(
) {
  {
    # Download if not already done during a previous run
    dir.create( "~/.keras/datasets/cifar-10-batches-py/"
                , recursive = TRUE, showWarnings = FALSE )
    if( !file.exists( "~/.keras/datasets/cifar-10-batches-py.tar.gz" ) ) {
      t1 <- proc.time()
      cat( "Downloading source data.." )
      download.file(url = "http://www.cs.toronto.edu/~kriz/cifar-10-python.tar.gz"
                    , destfile = "~/.keras/datasets/cifar-10-batches-py.tar.gz"
                    , mode = 'wb')
      cat( " done (", duration_string( t1 ), ").\n" )
    }
    if( !file.exists( "~/.keras/datasets/cifar-10-batches-py/data_batch_1" ) ) {
      t1 <- proc.time()
      cat( "Decompressing source data.." )
      unzip( "~/.keras/datasets/cifar-10-batches-py.tar.gz"
             , exdir = "~/.keras/datasets/cifar-10-batches-py" )
      cat( " done (", duration_string( t1 ), ").\n" )
    }
  }
  
  result <- list()
  {
    # A much corrected version of the solution introduced there =>
    # @see 'https://stackoverflow.com/questions/32113942/importing-cifar-10-data-set-to-r'
    
    t1 <- proc.time()
    cat( paste0(
      "Reading binary source files :\n|"
      , paste( rep( "=", 6 * ( 5 + 1 ) ), collapse = "" )
      , "|\n|" ) )
    files <-
      c(
        sapply(1:5, function(f) paste0(
          "~/.keras/datasets/cifar-10-batches-py/data_batch_"
          , f
        ) )
        , "~/.keras/datasets/cifar-10-batches-py/test_batch" )
    result$train$x <- array( NA_integer_, dim = c( 50000, 32, 32, 3 ) )
    result$train$y <- matrix( data = NA_integer_, nrow = 50000, ncol = 1 )
    result$test$x <- array( NA_integer_, dim = c( 10000, 32, 32, 3 ) )
    result$test$y <- matrix( data = NA_real_, nrow = 10000, ncol = 1 )
    file_imgCount = 10000 # Set to 10000 to retrieve all images per file to memory
    
    # Cycle through all 5 binary files
    for( f in 1:6 ) {
      to.read <-
        file(
          files[ f ]
          , "rb" )
      
      # accounting for a 'labels' block header
      # "batch_label training batch 1 of 5 labels"
      # which we skip over :
      readBin(to.read, integer(), endian = "big", size = 1
              , n = ifelse( f == 6, 59, 60 ) )
      
      ## labels block ##
      l <- readBin(
        to.read, integer(), endian = "big", size = 1
        , n = 2 * ( file_imgCount + ( file_imgCount / 1000 ) ) )
      # records are separated by a "75" value, which we ignore
      # additionally, every 1,000 records,
      # there is a "101" & "40" separator, which we also ignore
      l[ seq(2, length( l ), 2 ) ] -> l
      l[ -seq(1001, length( l ), 1001 ) ] -> l
      cat( "-" )
      
      # accounting for a 'data' block header
      # "data cnumpy.core.multiarray _reconstruct cnumpy array numpy type"
      # which we skip over :
      readBin(
        to.read, integer(), size = 1, n = 138, endian = "big" )
      
      ## data block ##
      for( i in 1:file_imgCount ) {
        # Cycle through all 'file_imgCount' images
        
        r <- as.integer( readBin(
          to.read, raw(), endian = "big", size = 1, n = 1024 ) )
        g <- as.integer( readBin(
          to.read, raw(), endian = "big", size = 1, n = 1024 ) )
        b <- as.integer( readBin(
          to.read, raw(), endian = "big", size = 1, n = 1024 ) )
        
        index <- file_imgCount * ( f - 1 ) + i
        if( f == 6 ) {
          result$test$x[ i, , , ] <-
            sapply(
              list(
                matrix( r, ncol = 32, byrow = TRUE )
                ,  matrix( g, ncol = 32, byrow = TRUE )
                ,  matrix( b, ncol = 32, byrow = TRUE )
              )
              , identity, simplify = "array" )
        } else {
          result$train$x[ index, , , ] <-
            sapply(
              list(
                matrix( r, ncol = 32, byrow = TRUE )
                ,  matrix( g, ncol = 32, byrow = TRUE )
                ,  matrix( b, ncol = 32, byrow = TRUE )
              )
              , identity, simplify = "array" )
        }
        
        if( i %% ceiling( file_imgCount / 5 ) == 0 &
            !( f == 6 & i == file_imgCount ) ) cat( "-" )
      }
      if( f == 6 ) {
        l ->
          result$test$y[
            , 1 ]
      } else {
        l ->
          result$train$y[
            ( ( f - 1 ) * file_imgCount + 1 ):
              ( f * file_imgCount )
            , 1 ]
      }
      
      close( to.read )
      rm( l, r, g, b, f, i, index, to.read )
    }
    cat( paste0(
      "-| done (", duration_string( t1 ), ").\n" ) )
    rm( t1 )
    # str( result )
  }
  return( result )
}
######################################################
# all.equal( amc_dataset_cifar10, keras::dataset_cifar10 ) == > TRUE




##############################
## SOURCE DATA FORMAT BEGIN ##
##############################
{
  if( require( keras ) ) {
    cifar10 <- keras::dataset_cifar10()
  } else {
    cifar10 <- amc_dataset_cifar10()
  }
  # list.files(path = "~/.keras/datasets/cifar-10-batches-py", full.name = TRUE )


  # Normalisation
  mea <- numeric( 3 )
  sds <- numeric( 3 )
  for( i in 1:3 ){
    mea[ i ] <- mean( cifar10$train$x[ , , , i ] )
    sds[ i ] <- sd( cifar10$train$x[ , , , i ] )
  
    cifar10$train$x[ , , , i ] <-
      ( cifar10$train$x[ , , , i ] - mea[ i ] ) / sds[ i ]
    cifar10$test$x[ , , , i ] <-
      ( cifar10$test$x[ , , , i ] - mea[ i ] ) / sds[ i ]
  }
  x_train <- cifar10$train$x
  x_test <- cifar10$test$x

  y_train <- cifar10$train$y[ , 1 ]
  y_test <- cifar10$test$y[ , 1 ]

  rm( cifar10 )
  gc( reset = FALSE, full = TRUE, verbose = FALSE )

  class_names <-
    bind_cols(
      class_id = 0:9
      , class = c( 'airplane',
                   'automobile',
                   'bird',
                   'cat',
                   'deer', 
                   'dog',
                   'frog',
                   'horse',
                   'ship',
                   'truck' ) )
}
##############################
##  SOURCE DATA FORMAT END  ##
##############################




## ############################################
##     'cifar10_tensor_to_image' function    ##
###############################################
# inputs :                                    #
#    - "x" - the source tensor in the form of #
#      a 3D array :                           #
#        - 'image width' x                    #
#        - 'image height' x                   #
#        - 'depth (nb of color layers)'       #
# to be considered                            #
###############################################
# returns a raster object (bitmap image)      #
###############################################
cifar10_tensor_to_image <- function(
  tensor_
){
  # De-normalize pixels
  tensor_[ , , 1 ] <-
    tensor_[ , , 1 ] * sds[ 1 ] + mea[ 1 ]
  tensor_[ , , 2 ] <-
    tensor_[ , , 2 ] * sds[ 2 ] + mea[ 2 ]
  tensor_[ , , 3 ] <-
    tensor_[ , , 3 ] * sds[ 3 ] + mea[ 3 ]

  # scale to [0-1] interval
  tensor_[] <- as.integer( tensor_ ) / 255
  tensor_
}
###############################################
# plot( as.raster( cifar10_tensor_to_image( x_train[ 50000, , , ] ) ) )




## ############################################
##        'get_cifar10_image' function       ##
###############################################
# inputs :                                    #
#    - "images" - the source data in the form #
#      of a 4D array :                        #
#        - 'nb of records' x                  #
#        - 'image width' x                    #
#        - 'image height' x                   #
#        - 'depth (nb of color layers)'       #
#    - "img_idx" the index of the image       #
#      to be considered                       #
###############################################
# returns a raster object (bitmap image)      #
###############################################
get_cifar10_image <- function(
  images
  , img_idx
){
  return(
    cifar10_tensor_to_image(
      images[ img_idx, , , ] ) )
}
###############################################
# plot( as.raster( get_cifar10_image( x_train, 50000 ) ) )




## #################################################
##          'plot_cifar10_image' function         ##
####################################################
# inputs :                                         #
#    - "images" - the source data in the form of   #
#      a 4D array :                                #
#        - 'nb of records' x                       #
#        - 'image width' x                         #
#        - 'image height' x                        #
#        - 'depth (nb of color layers)'            #
#    - "labels" a vector of length 'nb of records' #
#      integer values representing the 'classe id' #
#      in the [0_9] range                          #
#    - "img_idx" the index of the image            #
#      to be considered                            #
#    - "predictions" (optional) matrix             #
#      of prediction probabilities (for labelling) #
####################################################
# returns a "ggplot2" plot object                  #
# representing a plotted (labelled) image          #
####################################################
plot_cifar10_image <- function(
  images
  , labels
  , img_idx
  , predictions = NULL
){
  # Inserting a raster image to ggplot2
  # @see 'https://stackoverflow.com/questions/9917049/inserting-an-image-to-ggplot2'
  img <-
    qplot() +
    annotation_custom(
      rasterGrob( get_cifar10_image( images, img_idx )
                  , interpolate = TRUE )
      , xmin = -Inf, xmax = Inf
      , ymin = -Inf, ymax = Inf )
  if( is.null( predictions ) ) {
    img +
      xlab( class_names[
        labels[ img_idx ] + 1, ]$class ) ->
      img
  } else {
    if(
      labels[ img_idx ] ==
      which.max( cifar10_predictions[ img_idx, ] ) - 1
    ) {
      # case 'predicted class' is 'true class' :
      img +
        xlab( paste(
          class_names[
            labels[ img_idx ] + 1, ]$class
          , scales::percent(
            cifar10_predictions[
              img_idx
              , labels[ img_idx ] + 1 ]
            , accuracy = 1 )
          ) ) ->
        img
    } else {
      # case 'else' :
      img +
        xlab( paste(
          class_names[
            labels[ img_idx ] + 1, ]$class
          , scales::percent(
            cifar10_predictions[ img_idx
                                 , labels[ img_idx ] + 1 ]
            , accuracy = 1
          )
          , " ; "
          , class_names[
            which.max( cifar10_predictions[ img_idx, ] ), ]$class
          , scales::percent(
            cifar10_predictions[
              img_idx
              , which.max( cifar10_predictions[ img_idx, ] ) ]
            , accuracy = 1 )
          ) ) ->
        img
    }
  }
  return( img )
}
####################################################
# plot_cifar10_image( x_train, y_train, 6 )
# plot_cifar10_image( x_test, y_test, 9, cifar10_predictions )
#

if( FALSE ) {
  ## Not run:

  # Model Definition -------------------------------------------------------

  input_img <- layer_input( shape = c( 32, 32, 3 ) )

  cifar10_model <-
    application_densenet( include_top = TRUE
                          , input_tensor = input_img
                          , dropout_rate = 0.2 )
  opt <-
    optimizer_sgd( lr = 0.1
                   , momentum = 0.9
                   , nesterov = TRUE )

  cifar10_model %>% compile(
    optimizer = opt,
    loss = "categorical_crossentropy",
    metrics = "accuracy"
  )

  # callbacks for weights and learning rate
  lr_schedule <- function( epoch, lr ) {
    if( epoch <= 150 ) {
      0.1
    } else if( epoch > 150 && epoch <= 225 ){
      0.01
    } else {
      0.001
    }
  }

  lr_reducer <-
    callback_learning_rate_scheduler(
      lr_schedule )

  # Parameters --------------------------------------------------------------

  batch_size <- 64
  epochs <- 300

  # Model fitting -----------------------------------------------------------

  cifar10_history <- cifar10_model %>% fit(
    x = x_train
    , y = to_categorical( y_train
                          , num_classes = 10 )
    , batch_size = batch_size
    , epochs = epochs
    , validation_data =
      list( x_test
            , to_categorical( y_test
                              , num_classes = 10 ) )
    , callbacks =
      list( lr_reducer )
  )
  dir.create( "./models", showWarnings = FALSE )
  cifar10_model %>% save_model_hdf5( "./models/cifar10_model.h5" ) # saves the model (incl. weights) ; @see 'https://tensorflow.rstudio.com/keras/articles/tutorial_save_and_restore.html'
  # cifar10_model <- load_model_hdf5( "./models/cifar10_model.h5" )
  saveRDS( cifar10_history, "./models/cifar10_history" )
  # cifar10_history <- readRDS( "./models/cifar10_history" )

  # Predicting & evaluating ------------------------------------------------

  scores <-
    evaluate( cifar10_model
              , x_test, to_categorical( y_test
                                        , num_classes = 10 ) )
  cat( "Test loss:", scores[[ 1 ]], "\n" )
  cat( "Test accuracy:", scores[[ 2 ]], "\n" )

  # Generate predictions on new data:
  cifar10_predictions <-
    cifar10_model %>% predict( x = x_test )
  saveRDS( cifar10_predictions, "./models/cifar10_predictions" )
  # cifar10_predictions <- readRDS( "./models/cifar10_predictions" )
  #

  ## End(Not run)
}




# !!!!!!!!!!!!!!!!! CIFAR-10 SECTION END !!!!!!!!!!!!!!!!




# !!!!!!!!!!!!! STYLE TRANSFER SECTION BEGIN !!!!!!!!!!!!




if( FALSE ) {
  ## Not run:

  # Parameters --------------------------------------------------------------

  base_image_path <- "./images/style_transfer/neural-style-base-img.jpg"
  style_reference_image_path <- "./images/style_transfer/neural-style-snowy_wood-style.jpg"
  # style_reference_image_path <- "./images/style_transfer/neural-style-beach-style.jpg"
  iterations <- 10

  # these are the weights of the different loss components
  total_variation_weight <- 1
  style_weight <- 1
  content_weight <- 0.025

  # dimensions of the generated picture.
  img <- image_load( base_image_path )
  width <- img$size[[ 1 ]]
  height <- img$size[[ 2 ]]
  # img$size[[ 2 ]] # 3 ; BGR channels
  img_nrows <- 400
  img_ncols <- as.integer( width * img_nrows / height )




  # Functions ---------------------------------------------------------------

  ## ##################################################
  ## 'style_transfer_preprocess_image' function      ##
  #####################################################
  # util function to open, resize and format pictures #
  # into appropriate tensors                          #
  #####################################################
  style_transfer_preprocess_image <- function(
    path
  ){
    img <-
      image_load( path
                  , target_size =
                    c( img_nrows, img_ncols ) ) %>%
      image_to_array() %>%
      array_reshape( c( 1, dim( . ) ) )
    
    # centers & scales pixels
    imagenet_preprocess_input( img )
  }
  #####################################################




  ## ############################################
  ## 'style_transfer_deprocess_image' function ##
  ###############################################
  # util function to convert a tensor           #
  # into a valid image                          #
  # (also turn BGR into RGB).                   #
  ###############################################
  style_transfer_deprocess_image <- function(
    x
  ){
    x <- x[ 1, , , ]
    # Remove zero-center by mean pixel
    x[ , , 1 ] <- x[ , , 1 ] + 103.939
    x[ , , 2 ] <- x[ , , 2 ] + 116.779
    x[ , , 3 ] <- x[ , , 3 ] + 123.68
    # BGR -> RGB
    x <- x[ , , c( 3, 2, 1 ) ]
    # clip to interval 0, 255
    x[ x > 255 ] <- 255
    x[ x < 0 ] <- 0
    x[] <- as.integer( x ) / 255
    x
  }
  ###############################################




  # Defining the vgg16_model ------------------------------------------------

  # get tensor representations of our images
  base_image <-
    k_variable(
      style_transfer_preprocess_image( base_image_path ) )
  style_reference_image <-
    k_variable(
      style_transfer_preprocess_image( style_reference_image_path ) )
  
  # this will contain our generated image
  combination_image <-
    k_placeholder( c( 1, img_nrows, img_ncols, 3 ) )

  # combine the 3 images into a single Keras tensor
  input_tensor <-
    k_concatenate( list( base_image
                         , style_reference_image
                         ,  combination_image )
                   , axis = 1 )

  # build the VGG16 network with our 3 images as input
  # the vgg16_model will be loaded with pre-trained ImageNet weights
  vgg16_model <- application_vgg16( input_tensor = input_tensor
                                    , weights = "imagenet"
                                    ,  include_top = FALSE )
  # Downloads model data from https://github.com/fchollet/deep-learning-models/releases/download/v0.1/vgg16_weights_tf_dim_ordering_tf_kernels_notop.h5
  print( "Model loaded." )

  # export model summary
  # @see 'https://stackoverflow.com/questions/7096989/how-to-save-all-console-output-to-file-in-r'
  con <- file( "./models/vgg16_model_summary.txt" )
  sink( con, append = TRUE )
  summary( vgg16_model )
  # Restore output to console
  sink() 
  # cat( readLines( "./models/vgg16_model_summary.txt" ), sep = "\n" )




  nms <- map_chr( vgg16_model$layers, ~.x$name ) # model layer names
  output_dict <-
    map( vgg16_model$layers, ~.x$output ) %>%
    set_names( nms )





  # Compute the neural style loss -------------------------------------------
  # first we need to define 4 util functions




  ## ###################################
  ## 'gram_matrix' function           ##
  ######################################
  # the gram matrix of an image tensor #
  # (feature-wise outer product)       #
  ######################################
  gram_matrix <- function( x ){
    
    features <- x %>%
      k_permute_dimensions(
        pattern = c( 3, 1, 2 ) ) %>%
      k_batch_flatten()
    
    k_dot( features
           , k_transpose( features ) )
  }
  ######################################




  ## #########################################
  ## 'style_loss' function                  ##
  ############################################
  # the "style loss" is designed to maintain #
  # the style of the reference image         #
  # in the generated image.                  #
  # It is based on the gram matrices         #
  # (which capture style) of feature maps    #
  # from the style reference image           #
  # and from the generated image             #
  ############################################
  style_loss <- function(
    style
    , combination
  ){
    S <- gram_matrix( style )
    C <- gram_matrix( combination )
    
    channels <- 3
    size <- img_nrows * img_ncols
    
    k_sum(
      k_square( S - C ) ) /
      ( 4 * channels^2 * size^2 )
  }
  ############################################




  ## ###################################
  ## 'content_loss' function          ##
  ######################################
  # an auxiliary loss function         #
  # designed to maintain the "content" #
  # of the base image                  #
  # in the generated image             #
  ######################################
  content_loss <- function(
    base
    , combination
  ){
    k_sum( k_square( combination - base ) )
  }
  ######################################




  ## #############################################
  ## 'total_variation_loss' function            ##
  ################################################
  # the 3rd loss function, total variation loss, #
  # designed to keep the generated image         #
  # locally coherent                             #
  ################################################
  total_variation_loss <- function( x ){
    y_ij  <- x[ , 1:( img_nrows - 1L )
                , 1:( img_ncols - 1L ), ]
    y_i1j <- x[ , 2:( img_nrows )
                , 1:( img_ncols - 1L ), ]
    y_ij1 <- x[ , 1:( img_nrows - 1L )
                , 2:( img_ncols ), ]
    
    a <- k_square( y_ij - y_i1j )
    b <- k_square( y_ij - y_ij1 )
    k_sum( k_pow( a + b, 1.25 ) )
  }
  ################################################




  # combine these loss functions into a single scalar
  loss <- k_variable( 0.0 )
  layer_features <- output_dict$block4_conv2
  base_image_features <- layer_features[ 1, , , ]
  combination_features <- layer_features[ 3, , , ]

  loss <- loss +
    content_weight*content_loss( base_image_features
                                 , combination_features )

  feature_layers = c( 'block1_conv1', 'block2_conv1'
                      , 'block3_conv1', 'block4_conv1'
                      , 'block5_conv1' )

  for( layer_name in feature_layers ){
    layer_features <- output_dict[[ layer_name ]]
    style_reference_features <- layer_features[ 2, , , ]
    combination_features <- layer_features[ 3 , , , ]
    sl <- style_loss( style_reference_features
                      , combination_features )
    loss <- loss + ( ( style_weight /
                         length( feature_layers ) ) * sl )
  }

  loss <-
    loss + (
      total_variation_weight *
        total_variation_loss( combination_image )
    )

  # get the gradients of the generated image wrt the loss
  grads <-
    k_gradients( loss, combination_image )[[ 1 ]]

  f_outputs <-
    k_function( list( combination_image )
                , list( loss, grads ) )




  ## #################################
  ## 'eval_loss_and_grads' function ##
  ####################################
  eval_loss_and_grads <-
    function( image ){
      image <- array_reshape( image
                              , c( 1
                                   , img_nrows
                                   , img_ncols, 3 ) )
      outs <- f_outputs( list( image ) )
      list(
        loss_value = outs[[ 1 ]],
        grad_values =
          array_reshape( outs[[ 2 ]]
                         , dim = length( outs[[ 2 ]]
                         ) )
      )
    }
  ####################################




  # Loss and gradients evaluator.
  # 
  # This EvaluatorClass class makes it possible
  # to compute loss and gradients in one pass
  # while retrieving them via two separate functions,
  # "loss" and "grads". This is done because scipy.optimize
  # requires separate functions for loss and gradients,
  # but computing them separately would be inefficient.
  EvaluatorClass <- R6Class(
    "EvaluatorClass"
    , public = list(
      loss_value = NULL
      , grad_values = NULL
      , initialize = function() {
        self$loss_value <- NULL
        self$grad_values <- NULL
      }
      , loss = function( x ){
        loss_and_grad <- eval_loss_and_grads( x )
        self$loss_value <- loss_and_grad$loss_value
        self$grad_values <- loss_and_grad$grad_values
        self$loss_value
      }
      , grads = function( x ){
        grad_values <- self$grad_values
        self$loss_value <- NULL
        self$grad_values <- NULL
        grad_values
      }
    )
  )
  evaluator <- EvaluatorClass$new()





  # run scipy-based optimization (L-BFGS)
  # over the pixels of the generated image
  # so as to minimize the neural style loss
  dms <- c( 1, img_nrows, img_ncols, 3 )
  x <- array( data = runif( prod( dms )
                            , min = 0
                            , max = 255 ) - 128
              , dim = dms )

  # Run optimization (L-BFGS) over the pixels
  # of the generated image
  # so as to minimize the loss
  t1 <- proc.time()
  for( i in 1:iterations ){
    
    # Run L-BFGS
    opt <- optim(
      array_reshape(
        x, dim = length( x ) )
      , fn = evaluator$loss
      , gr = evaluator$grads
      , method = "L-BFGS-B"
      , control = list( maxit = 15 )
    )
    
    # Print loss value
    cat( paste( i, "/", iterations, " - ", opt$value, "\n" ) )
    
    # decode the image
    image <- x <- opt$par
    image <- array_reshape( image, dms )
    
    # plot
    im <- style_transfer_deprocess_image(image)
    plot(as.raster(im))
  }
  cat( paste0(
    " done (", duration_string( t1 ), ")\n" ) )


  # Export "final-iteration" image to file
  # remove all plot margin :
  # @see 'https://stackoverflow.com/questions/5663888/trying-to-remove-all-margins-so-that-plot-region-comprises-the-entire-graphic#5664040'
  op <- par( mar = rep( 0, 4 ) )
  plot( as.raster( im ) )
  par( op )
  # export the plot to external file with 'png' image format :
  # dev.print( png, file = "./images/style_transfer/beach.png", width = width, height = height )#, bg="transparent" )
  dev.print( png, file = "./images/style_transfer/snowy_wood.png", width = width, height = height )#, bg="transparent" )
  rm( im )
## End(Not run)
}





# !!!!!!!!!!!!!! STYLE TRANSFER SECTION END !!!!!!!!!!!!!






# EXTRA (if encountering version compatibility issues between
# keras/tensorflow/CUDA/CuDNN


# @see 'https://conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html#activating-an-environment'
#
# List the available conda environments :
# conda info --envs
#
# To use pip in your environment, in your terminal window or an Anaconda Prompt, run:
# conda install -n r-tensorflow pip
#
# From the Anaconda Prompt ;
# activate one of the environments
# conda activate r-tensorflow
#
# list the packages in this environment :
# conda list -n r-tensorflow

# conda remove tensorflow

# conda remove tensorflow
# conda install tensorflow-gpu
# pip install --ignore-installed --upgrade tensorflow-gpu

# conda deactivate

# conda install -c anaconda cudnn==7.4.1
# 
# conda install tensorflow-gpu==1.12.0


# Upgrade the CuDNN version used by Anaconda
# (only python distribution supported by the 'keras'pakage on Windows) :
# @see 'https://medium.com/@shaolinkhoa/install-tensorflow-gpu-2-0-alpha-on-anaconda-for-windows-10-ubuntu-ced099010b21'
# From :
# C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v10.1\tools\cuDNN\bin
# To :
# C:\Users\Organization\Anaconda3\envs\r-tensorflow\Library\bin
#
# From :
# C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v10.1\tools\cuDNN\include
# To :
# C:\Users\Organization\Anaconda3\envs\r-tensorflow\Library\include
#
# From :
# C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v10.1\tools\cuDNN\lib\x64
# To :
# C:\Users\Organization\Anaconda3\envs\r-tensorflow\Library\lib\x64






















































