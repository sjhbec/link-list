#------------------------------------------------------------------------------
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)
#==============================================================================
PROJECT(igafem)
ENABLE_LANGUAGE(Fortran)

#==============================================================================
SET(${PROJECT_NAME}_VERSION_MAJOR "0")
SET(${PROJECT_NAME}_VERSION_MINOR "0")
SET(${PROJECT_NAME}_VERSION_PATCH "1")

SET(VERSION
"${PROJECT_NAME}_VERSION_MAJOR.${PROJECT_NAME}_VERSION_MINOR.${PROJECT_NAME}_VERSION_PATCH")
#==============================================================================


IF(NOT EXECUTABLE_OUTPUT_PATH)
SET( EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin )
ENDIF(NOT EXECUTABLE_OUTPUT_PATH)

IF(NOT LIBRARY_OUTPUT_PATH)
  SET( LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib )
ENDIF(NOT LIBRARY_OUTPUT_PATH)

IF( NOT CMAKE_Fortran_MODULE_DIRECTORY )
SET (CMAKE_Fortran_MODULE_DIRECTORY  ${PROJECT_BINARY_DIR}/lib/mod_files CACHE PATH "Single Directory for all fortran modules." )
ENDIF(NOT CMAKE_Fortran_MODULE_DIRECTORY )


OPTION(BUILD_SHARED_LIBS "Build shared librarires" ON )

set( input_config_file "config_dir/config.in" )

OPTION( DISABLE_DEFERRED_CHARACTER_STRING     "Disable Deferred-length character " ON )


if(DISABLE_DEFERRED_CHARACTER_STRING)
add_definitions(-DDISABLE_DEFERRED_CHARACTER)
endif(DISABLE_DEFERRED_CHARACTER_STRING)

#==============================================================================

LINK_DIRECTORIES( ${THIS_LINK_DIRS} )

#==============================================================================

#==============================================================================
# ------------------- add isubdirectory for this project --------------------

ADD_SUBDIRECTORY( src    )
ADD_SUBDIRECTORY( driver )

#==============================================================================

# ---------------------------  CONFIGURE INSTALL ------------------------------
#==============================================================================


