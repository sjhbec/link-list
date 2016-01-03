#==============================================================================
PROJECT( Linklist_Library )
ENABLE_LANGUAGE(Fortran)
#==============================================================================

# --------------------------  define sources  ----------------------------------

file( GLOB LinkList_FILES         "My_Datatypes.f90" "Module_String_Tools.f90" "Module_Type_Conversion.f90" "Class_List.f90"  )

#==============================================================================

SET ( LinkList_SRCS          ${LinkList_FILES}    )

#==============================================================================

INCLUDE_DIRECTORIES( ${THIS_INCLUDE_DIRS} )

LINK_DIRECTORIES( ${THIS_LINK_DIRS} )

ADD_LIBRARY ( linklist ${LinkList_SRCS} )
TARGET_LINK_LIBRARIES( linklist ${THIS_LINK_LIBS} )
#==============================================================================

# ---------------------------  CONFIGURE INSTALL ------------------------------
#==============================================================================

INSTALL ( TARGETS linklist
          RUNTIME DESTINATION bin
          LIBRARY DESTINATION lib
          ARCHIVE DESTINATION lib/static
        )

#==============================================================================
#------------------------------------------------------------------------------

