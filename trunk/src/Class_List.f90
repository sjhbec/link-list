!******************************************************************************
       MODULE CLASS_G_LIST
!******************************************************************************
         USE My_Datatypes
         USE String_Tools, ONLY : LEN_BTRIM, BTRIM, STRCMP, INT_TO_STR
         USE Type_Conversion
         IMPLICIT NONE
         PRIVATE
 
         PUBLIC :: G_LINK, G_LIST

!------------------------------------------------------------------------------
         CHARACTER(22), PARAMETER :: CLASS_NAME = &
                                             "'CLASS_G_LIST'"
         CHARACTER(MAX_PROCEDURE_LEN) :: PROCEDURE_NAME = ''
         CHARACTER(MAX_NAME_LEN) :: LOCAL_FLAG = ''
         CHARACTER(1), TARGET :: Null_Key = NullChar
         
         INTEGER, TARGET :: Null_Value = 0
         INTEGER, TARGET :: Null_V_VAL(1) = 0
         INTEGER, TARGET :: Null_M_VAL(1,1) = 0
         INTEGER, PARAMETER :: Header_Len = 5 * Int_Byte

!|---------------------------------------------------------------------------------|
!|---------------------------------------------------------------------------------|
!|                                                                                 |
!|           - - - - - - D E S C R I P T I O N  O F  L I N K - - - - -             |
!|                                                                                 |
!|  Total bytes = 4* sizeof(integer) + key_lentgh + sizeof(value )                 |
!|  4 integers are reserved for Content of This Link                               |
!|         Content(1) = N_Row of Data                                              |
!|         Content(2) = N_Col of Data                                              |
!|         Content(3) = No of Characters in Key                                    |
!|         Content(4) = Type of stored data                                        |
!|                                                                                 |
!|            0 : Character(1)   1 bytes                                           |
!|            1 : Integer(1)     1 bytes                                           |
!|            2 : Integer(2)     2 bytes                                           |
!|            4 : Integer(4)     4 bytes                                           |
!|            8 : Integer(8)     8 bytes                                           |
!|            11 : Real(4)       4 bytes                                           |
!|            12 : Real(8)       8 bytes                                           |
!|            13 : Real(16)     16 bytes                                           |
!|            21 : Complex(4)    8 bytes                                           |
!|            22 : Complex(8)   16 bytes                                           |
!|            23 : Complex(16)  32 bytes                                           |
!|                                                                                 |
!|---------------------------------------------------------------------------------|
!|---------------------------------------------------------------------------------|
 
        TYPE G_LINK
           PRIVATE
           INTEGER(1), DIMENSION(:), Allocatable :: Value  ! FOR Storing integer(1) Casted ( Content + Values + Key )
           TYPE(G_LINK), POINTER :: NEXT => NULL() ! NEXT LINK IN LIST
           CONTAINS
           PRIVATE
           PROCEDURE, PUBLIC :: GIVE_CONTENT
           PROCEDURE, PRIVATE :: SET_LINK_KEY
           GENERIC, PUBLIC :: SET_KEY => SET_LINK_KEY
           PROCEDURE, PUBLIC :: GIVE_KEY
           PROCEDURE, PRIVATE :: INSERT_SS_DATA
           PROCEDURE, PRIVATE :: INSERT_SI1_DATA
           PROCEDURE, PRIVATE :: INSERT_SI2_DATA
           PROCEDURE, PRIVATE :: INSERT_SI4_DATA
           PROCEDURE, PRIVATE :: INSERT_SI8_DATA
           PROCEDURE, PRIVATE :: INSERT_SR4_DATA
           PROCEDURE, PRIVATE :: INSERT_SR8_DATA
           PROCEDURE, PRIVATE :: INSERT_SR16_DATA
           PROCEDURE, PRIVATE :: INSERT_SC4_DATA
           PROCEDURE, PRIVATE :: INSERT_SC8_DATA
           PROCEDURE, PRIVATE :: INSERT_SC16_DATA
           PROCEDURE, PRIVATE :: INSERT_VI1_DATA
           PROCEDURE, PRIVATE :: INSERT_VI2_DATA
           PROCEDURE, PRIVATE :: INSERT_VI4_DATA
           PROCEDURE, PRIVATE :: INSERT_VI8_DATA
           PROCEDURE, PRIVATE :: INSERT_VR4_DATA
           PROCEDURE, PRIVATE :: INSERT_VR8_DATA
           PROCEDURE, PRIVATE :: INSERT_VR16_DATA
           PROCEDURE, PRIVATE :: INSERT_VC4_DATA
           PROCEDURE, PRIVATE :: INSERT_VC8_DATA
           PROCEDURE, PRIVATE :: INSERT_VC16_DATA
           PROCEDURE, PRIVATE :: INSERT_MI1_DATA
           PROCEDURE, PRIVATE :: INSERT_MI2_DATA
           PROCEDURE, PRIVATE :: INSERT_MI4_DATA
           PROCEDURE, PRIVATE :: INSERT_MI8_DATA
           PROCEDURE, PRIVATE :: INSERT_MR4_DATA
           PROCEDURE, PRIVATE :: INSERT_MR8_DATA
           PROCEDURE, PRIVATE :: INSERT_MR16_DATA
           PROCEDURE, PRIVATE :: INSERT_MC4_DATA
           PROCEDURE, PRIVATE :: INSERT_MC8_DATA
           PROCEDURE, PRIVATE :: INSERT_MC16_DATA
           GENERIC, PUBLIC :: INSERT_DATA =>  &
                              INSERT_SS_DATA,      &
                              INSERT_SI1_DATA, INSERT_SI2_DATA, INSERT_SI4_DATA, INSERT_SI8_DATA,  & 
                              INSERT_SR4_DATA, INSERT_SR8_DATA, INSERT_SR16_DATA, &
                              INSERT_SC4_DATA, INSERT_SC8_DATA, INSERT_SC16_DATA, &
                              INSERT_VI1_DATA, INSERT_VI2_DATA, INSERT_VI4_DATA, INSERT_VI8_DATA,  & 
                              INSERT_VR4_DATA, INSERT_VR8_DATA, INSERT_VR16_DATA, &
                              INSERT_VC4_DATA, INSERT_VC8_DATA, INSERT_VC16_DATA, &
                              INSERT_MI1_DATA, INSERT_MI2_DATA, INSERT_MI4_DATA, INSERT_MI8_DATA,  & 
                              INSERT_MR4_DATA, INSERT_MR8_DATA, INSERT_MR16_DATA, &
                              INSERT_MC4_DATA, INSERT_MC8_DATA, INSERT_MC16_DATA
           PROCEDURE, PRIVATE :: Link_Data_Type 
           GENERIC, PUBLIC :: DATA_TYPE => Link_Data_Type
           PROCEDURE, PRIVATE :: GIVE_DATA_SIZE_OF_LINK 
           GENERIC, PUBLIC :: DATA_SIZE => GIVE_DATA_SIZE_OF_LINK
           PROCEDURE, PUBLIC :: GIVE_SS_Data
           PROCEDURE, PUBLIC :: GIVE_SI1_Data
           PROCEDURE, PUBLIC :: GIVE_SI2_Data
           PROCEDURE, PUBLIC :: GIVE_SI4_Data
           PROCEDURE, PUBLIC :: GIVE_SI8_Data
           PROCEDURE, PUBLIC :: GIVE_SR4_Data
           PROCEDURE, PUBLIC :: GIVE_SR8_Data
           PROCEDURE, PUBLIC :: GIVE_SR16_Data
           PROCEDURE, PUBLIC :: GIVE_SC4_Data
           PROCEDURE, PUBLIC :: GIVE_SC8_Data
           PROCEDURE, PUBLIC :: GIVE_SC16_Data
           PROCEDURE, PUBLIC :: GIVE_VI1_Data
           PROCEDURE, PUBLIC :: GIVE_VI2_Data
           PROCEDURE, PUBLIC :: GIVE_VI4_Data
           PROCEDURE, PUBLIC :: GIVE_VI8_Data
           PROCEDURE, PUBLIC :: GIVE_VR4_Data
           PROCEDURE, PUBLIC :: GIVE_VR8_Data
           PROCEDURE, PUBLIC :: GIVE_VR16_Data
           PROCEDURE, PUBLIC :: GIVE_VC4_Data
           PROCEDURE, PUBLIC :: GIVE_VC8_Data
           PROCEDURE, PUBLIC :: GIVE_VC16_Data
           PROCEDURE, PUBLIC :: GIVE_MI1_Data
           PROCEDURE, PUBLIC :: GIVE_MI2_Data
           PROCEDURE, PUBLIC :: GIVE_MI4_Data
           PROCEDURE, PUBLIC :: GIVE_MI8_Data
           PROCEDURE, PUBLIC :: GIVE_MR4_Data
           PROCEDURE, PUBLIC :: GIVE_MR8_Data
           PROCEDURE, PUBLIC :: GIVE_MR16_Data
           PROCEDURE, PUBLIC :: GIVE_MC4_Data
           PROCEDURE, PUBLIC :: GIVE_MC8_Data
           PROCEDURE, PUBLIC :: GIVE_MC16_Data
           PROCEDURE, PRIVATE :: PRINT_LINK_VALUE
           GENERIC, PUBLIC :: PRINT_VALUE => PRINT_LINK_VALUE
           PROCEDURE, PUBLIC :: GIVE_NEXT    ! RETURN NEXT POINTER
           PROCEDURE, PUBLIC :: SET_NEXT ! SET NEXT POINTER
           PROCEDURE, PUBLIC :: SET_NEXT_NULL ! SET NEXT POINTER TO NULL()
           PROCEDURE, PRIVATE :: ERASE_LINK ! ERASE_LINK LINK
           GENERIC, PUBLIC :: ERASE => ERASE_LINK
           PROCEDURE, NOPASS, PUBLIC :: ERROR_CODE
           END TYPE G_LINK
 
 
!------------------------------------------------------------------------------
        INTERFACE G_LINK
        MODULE PROCEDURE SET_LINK    ! default constructor; only allocates memory
        END INTERFACE G_LINK
 
!******************************************************************************
 
         TYPE G_LIST
            PRIVATE
            INTEGER, POINTER :: Data_t
            TYPE(G_LINK), POINTER :: FIRST => NULL()! FIRST LINK IN LIST
            TYPE(G_LINK), POINTER :: LAST => NULL() ! LAST LINK IN LIST
         CONTAINS
         PRIVATE
            PROCEDURE, PUBLIC :: SET_LIST_TYPE
            PROCEDURE, PUBLIC :: PRINT_VALUE! PRINT LINK AT LEY OF CURRENT LIST
            PROCEDURE, PUBLIC :: PRINT_LIST! PRINT LINKED LIST
            PROCEDURE, PRIVATE :: PUSH_FRONT_S
            PROCEDURE, PRIVATE :: PUSH_FRONT_V
            PROCEDURE, PRIVATE :: PUSH_FRONT_M
            GENERIC, PUBLIC :: PUSH_FRONT => PUSH_FRONT_S, PUSH_FRONT_V, PUSH_FRONT_M
            PROCEDURE, PRIVATE :: PUSH_BACK_S
            PROCEDURE, PRIVATE :: PUSH_BACK_V
            PROCEDURE, PRIVATE :: PUSH_BACK_M
            GENERIC, PUBLIC :: PUSH_BACK => PUSH_BACK_S, PUSH_BACK_V, PUSH_BACK_M
            PROCEDURE, PRIVATE :: INSERT_FRONT_S
            PROCEDURE, PRIVATE :: INSERT_FRONT_V
            PROCEDURE, PRIVATE :: INSERT_FRONT_M
            GENERIC, PUBLIC :: INSERT_FRONT => INSERT_FRONT_S, INSERT_FRONT_V, INSERT_FRONT_M
            PROCEDURE, PRIVATE :: INSERT_BACK_S
            PROCEDURE, PRIVATE :: INSERT_BACK_V
            PROCEDURE, PRIVATE :: INSERT_BACK_M
            GENERIC, PUBLIC :: INSERT_BACK => INSERT_BACK_S, INSERT_BACK_V, INSERT_BACK_M
            PROCEDURE, PUBLIC :: IS_EMPTY
            PROCEDURE, PUBLIC :: HAS_KEY
            PROCEDURE, PRIVATE :: GIVE_LINK_AT
            GENERIC, PUBLIC :: GIVE_LINK => GIVE_LINK_AT
            PROCEDURE, PRIVATE :: ERASE_LINK_AT
            GENERIC, PUBLIC :: ERASE_VALUE => ERASE_LINK_AT
            PROCEDURE, PUBLIC :: ERASE
            PROCEDURE, NOPASS, PUBLIC :: ERROR_CODE
          END TYPE G_LIST
 
 
!******************************************************************************
         CONTAINS
!******************************************************************************
 
!----------------------------[ LINK FUNCTIONS ARE DEFINED HERE ]--------------------------

!******************************************************************************
        FUNCTION SET_LINK( Key_Len, Data_Type, N_Row, N_Col )
!******************************************************************************
        IMPLICIT NONE
        INTEGER, INTENT( IN ) :: Key_Len
        INTEGER, INTENT( IN ) :: Data_Type
        INTEGER, INTENT( IN ) :: N_Row
        INTEGER, INTENT( IN ) :: N_Col
 
        TYPE(G_LINK), POINTER :: SET_LINK
        INTEGER :: Ini, Fnl, Link_Len

        PROCEDURE_NAME = "'SET_LINK'"
 
! - - - - - Check Association and Nullify if required - - - - - - - -
         IF( ASSOCIATED( SET_LINK ) ) NULLIFY( SET_LINK )
! - - - - - - - - - - - - - Allocate Link - - - - - - - - - - - - - -
         ALLOCATE(SET_LINK)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
 
!- - - - - - - - - - - Set Next Link to NULL() - - - - - - - - - - -
          SET_LINK%NEXT =>  NULL()
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!-- - - - - - Estimate Key Storage and Value Storage in bytes- - - - - -
         Link_Len = Header_Len + Key_Len

         SELECT CASE ( Data_Type )
         CASE(0:1)     !  Character(1) or INTEGER(1)
         Link_Len = Link_Len + N_Row * N_Col
         CASE( 2)      !  INTEGER(2)
         Link_Len = Link_Len + N_Row * N_Col * 2
         CASE( 3)      !  INTEGER(4)
         Link_Len = Link_Len + N_Row * N_Col * 4
         CASE( 4)      !  INTEGER(8)
         Link_Len = Link_Len + N_Row * N_Col * 8
         CASE(11)     !  REAL(4)
         Link_Len = Link_Len + N_Row * N_Col * 4
         CASE(12)     !  REAL(8)
         Link_Len = Link_Len + N_Row * N_Col * 8
         CASE(13)     !  REAL(16)
         Link_Len = Link_Len + N_Row * N_Col * 16
         CASE(21)     !  COMPLEX(4)
         Link_Len = Link_Len + N_Row * N_Col * 8
         CASE(22)     !  COMPLEX(8)
         Link_Len = Link_Len + N_Row * N_Col * 16
         CASE(23)     !  COMPLEX(16)
         Link_Len = Link_Len + N_Row * N_Col * 32
         CASE DEFAULT
       PROCEDURE_NAME = "'SET_LINK'"
       LOCAL_FLAG = "UNKNOWN DATA_TYPE SUPPLIED VALUE ="//INT_TO_STR(DATA_TYPE)
       WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )//" :: "
       WRITE(*,'(A)')BTRIM( LOCAL_FLAG )
       STOP
         END SELECT

!- - - - - - - - - - - - Allocate Link Storage - - - - - - - - - - - - -
         IF( ALLOCATED( SET_LINK%Value ) ) DEALLOCATE( SET_LINK%Value )
         ALLOCATE( SET_LINK%Value( Link_Len ) )
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

!- - - - - - - - - - - - Define Link Content - - - - - - - - - - - - -

!---------------------- Insert Value of Key_Len --------------------
         Ini = 1
         Fnl = Int_Byte
         CALL Integer_Cast( Key_Len, SET_LINK%Value( Ini: Fnl) ) 
!-----------------------------------------------------------------

!---------------------- Insert Value of Data Type ----------------
         Ini = Int_Byte + 1
         Fnl = 2 * Int_Byte
         CALL Integer_Cast( Data_Type, SET_LINK%Value( Ini: Fnl) ) 
!-----------------------------------------------------------------

!---------------------- Insert Value of N_Row --------------------
         Ini = 2 * Int_Byte + 1
         Fnl = 3 * Int_Byte
         CALL Integer_Cast( N_Row, SET_LINK%Value( Ini: Fnl) ) 
!-----------------------------------------------------------------

!---------------------- Insert Value of N_Col --------------------
         Ini = 3 * Int_Byte + 1
         Fnl = 4 * Int_Byte
         CALL Integer_Cast( N_Col, SET_LINK%Value( Ini: Fnl) ) 
!-----------------------------------------------------------------

!--------------------- Insert I_Starting Data Location ------------------------
         Ini = 4 * Int_Byte + 1
         Fnl = 5 * Int_Byte
         CALL Integer_Cast( Header_Len + Key_Len + 1, SET_LINK%Value( Ini: Fnl) ) 
!----------------------------------------------------------------------------
         
!******************************************************************************
        END FUNCTION SET_LINK
!******************************************************************************

!******************************************************************************
        SUBROUTINE Give_CONTENT( THIS, Key_Len, Data_Type, N_Row, N_Col, Ini_Data )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN OUT ) :: Key_Len
        INTEGER, INTENT( IN OUT ) :: Data_Type
        INTEGER, INTENT( IN OUT ) :: N_Row
        INTEGER, INTENT( IN OUT ) :: N_Col
        INTEGER, INTENT( IN OUT ) :: Ini_Data
 
        INTEGER :: Ini, Fnl

        IF( ALLOCATED( THIS%VALUE ) )THEN
        
        SELECT CASE( Int_Byte )
        CASE( I4P )

!------------------------- Get Key_Length -----------------------
         Ini = 1
         Fnl = Int_Byte
         Key_Len = Cast_to_I4( This%Value( Ini: Fnl) ) 
!----------------------------------------------------------------

!------------------------- Get Data Type -------------------------
         Ini = Int_Byte + 1
         Fnl = 2 * Int_Byte
         Data_Type = Cast_to_I4( This%Value( Ini: Fnl) ) 
!-----------------------------------------------------------------

!------------------------- Get N_Row -----------------------------
         Ini = 2 * Int_Byte + 1
         Fnl = 3 * Int_Byte
         N_Row = Cast_to_I4( This%Value( Ini: Fnl) ) 
!-----------------------------------------------------------------

!------------------------- Get N_Col -----------------------------
         Ini = 3 * Int_Byte + 1
         Fnl = 4 * Int_Byte
         N_Col = Cast_to_I4( This%Value( Ini: Fnl) ) 
!-----------------------------------------------------------------
         
!------------------ Get I_Starting Location of Value ----------------
         Ini = 4 * Int_Byte + 1
         Fnl = 5 * Int_Byte
         Ini_Data = Cast_to_I4( This%Value( Ini: Fnl) ) 
!----------------------------------------------------------------
         
        CASE( I8P )

!------------------------- Get Key_Length -----------------------
         Ini = 1
         Fnl = Int_Byte
         Key_Len = Cast_to_I8( This%Value( Ini: Fnl) ) 
!----------------------------------------------------------------

!------------------------- Get Data Type -------------------------
         Ini = Int_Byte + 1
         Fnl = 2 * Int_Byte
         Data_Type = Cast_to_I8( This%Value( Ini: Fnl) ) 
!-----------------------------------------------------------------

!------------------------- Get N_Row -----------------------------
         Ini = 2 * Int_Byte + 1
         Fnl = 3 * Int_Byte
         N_Row = Cast_to_I8( This%Value( Ini: Fnl) ) 
!-----------------------------------------------------------------

!------------------------- Get N_Col -----------------------------
         Ini = 3 * Int_Byte + 1
         Fnl = 4 * Int_Byte
         N_Col = Cast_to_I8( This%Value( Ini: Fnl) ) 
!-----------------------------------------------------------------
         
!------------------ Get I_Starting Location of Value ----------------
         Ini = 4 * Int_Byte + 1
         Fnl = 5 * Int_Byte
         Ini_Data = Cast_to_I8( This%Value( Ini: Fnl) ) 
!------------------------------------------------------------------
         
         END SELECT

         ELSE

       PROCEDURE_NAME = "'Give_Content'"
       LOCAL_FLAG = "THIS LINK DOES NOT EXIST"
       WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )//" :: "
       WRITE(*,'(A)')BTRIM( LOCAL_FLAG )//" :: "
       
       Key_Len   = - 1
       Data_Type = - 1
       N_Row     = - 1
       N_Col     = - 1
       Ini_Data  = - 1

         ENDIF

!******************************************************************************
        END SUBROUTINE Give_CONTENT
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE SET_LINK_KEY( THIS, New_Key )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        CHARACTER(LEN=*), INTENT( IN ) :: New_Key
 
        INTEGER :: Key_Len, Ini, Fnl

!- - - - - - - - - - - - INSERT Key to LINK  - - - - - - - - - - - - - -
        Key_Len = Transfer( This%Value( 1: Int_Byte), Key_Len ) 

        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        CALL String_Cast( New_Key, THIS%Value( Ini: Fnl ) ) 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE SET_LINK_KEY
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_SS_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        CHARACTER(LEN=*), INTENT( IN ) :: New_Value
 
        INTEGER :: Fnl

!- - - - - - - - - - - - INSERT String to LINK  - - - - - - - - - - - - - -
        Fnl = Ini + LEN( New_Value ) - 1
        CALL String_Cast( New_Value, THIS%Value( Ini: Fnl ) ) 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_SS_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_SI1_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        INTEGER(1), INTENT( IN ) :: New_Value
 
!- - - - - - - - - - - - INSERT INTEGER(1) to LINK  - - - - - - - - - - - - - -
        This%Value( Ini ) = Transfer( New_Value, This%Value( Ini ) )
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_SI1_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_SI2_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        INTEGER(2), INTENT( IN ) :: New_Value
 
        INTEGER :: Fnl

!- - - - - - - - - - - - INSERT INTEGER(2) to LINK  - - - - - - - - - - - - - -
        Fnl = Ini + 1
        This%Value( Ini: Fnl ) = Transfer( New_Value, This%Value( Ini: Fnl ) )
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_SI2_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_SI4_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        INTEGER(4), INTENT( IN ) :: New_Value
 
        INTEGER :: Fnl

!- - - - - - - - - - - - INSERT INTEGER(4) to LINK  - - - - - - - - - - - - - -
        Fnl = Ini + 3
        This%Value( Ini: Fnl ) = Transfer( New_Value, This%Value( Ini: Fnl ) )
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_SI4_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_SI8_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        INTEGER(8), INTENT( IN ) :: New_Value
 
        INTEGER :: Fnl

!- - - - - - - - - - - - INSERT INTEGER(8) to LINK  - - - - - - - - - - - - - -
        Fnl = Ini + 7
        This%Value( Ini: Fnl ) = Transfer( New_Value, This%Value( Ini: Fnl ) )
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_SI8_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_SR4_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        REAL(R4P), INTENT( IN ) :: New_Value
 
        INTEGER :: Fnl

!- - - - - - - - - - - - INSERT REAL(4) to LINK  - - - - - - - - - - - - - -
        Fnl = Ini + 3
        This%Value( Ini: Fnl ) = Transfer( New_Value, This%Value( Ini: Fnl ) )
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_SR4_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_SR8_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        REAL(R8P), INTENT( IN ) :: New_Value
 
        INTEGER :: Fnl

!- - - - - - - - - - - - INSERT REAL(8) to LINK  - - - - - - - - - - - - - -
        Fnl = Ini + 7
        This%Value( Ini: Fnl ) = Transfer( New_Value, This%Value( Ini: Fnl ) )
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_SR8_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_SR16_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        REAL(R16P), INTENT( IN ) :: New_Value
 
        INTEGER :: Fnl

!- - - - - - - - - - - - INSERT REAL(16) to LINK  - - - - - - - - - - - - - -
        Fnl = Ini + 15
        This%Value( Ini: Fnl ) = Transfer( New_Value, This%Value( Ini: Fnl ) )
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_SR16_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_SC4_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        COMPLEX(R4P), INTENT( IN ) :: New_Value
 
        INTEGER :: Fnl

!- - - - - - - - - - - - INSERT COMPLEX(4) to LINK  - - - - - - - - - - - - - -
        Fnl = Ini + 3
        This%Value( Ini: Fnl ) = Transfer( New_Value, This%Value( Ini: Fnl ) )
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_SC4_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_SC8_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        COMPLEX(R8P), INTENT( IN ) :: New_Value
 
        INTEGER :: Fnl

!- - - - - - - - - - - - INSERT COMPLEX(8) to LINK  - - - - - - - - - - - - - -
        Fnl = Ini + 7
        This%Value( Ini: Fnl ) = Transfer( New_Value, This%Value( Ini: Fnl ) )
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_SC8_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_SC16_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        COMPLEX(R16P), INTENT( IN ) :: New_Value
 
        INTEGER :: Fnl

!- - - - - - - - - - - - INSERT COMPLEX(16) to LINK  - - - - - - - - - - - - - -
        Fnl = Ini + 15
        This%Value( Ini: Fnl ) = Transfer( New_Value, This%Value( Ini: Fnl ) )
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_SC16_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_VI1_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        INTEGER(1), DIMENSION(:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, N_Row, I_Start

!- - - - - - - - - - - - INSERT INTEGER(1) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )

        I_Start = Ini
        DO I = 1, N_Row
        This%Value( I_Start ) = Transfer( New_Value( I ), This%Value( I_Start ) )
        I_Start = I_Start + 1
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_VI1_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_VI2_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        INTEGER(2), DIMENSION(:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, N_Row, I_Start, I_Stop

!- - - - - - - - - - - - INSERT INTEGER(2) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )

        I_Start = Ini
        DO I = 1, N_Row
        I_Stop = I_Start + 1
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 2
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_VI2_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_VI4_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        INTEGER(4), DIMENSION(:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, N_Row, I_Start, I_Stop

!- - - - - - - - - - - - INSERT INTEGER(4) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )

        I_Start = Ini
        DO I = 1, N_Row
        I_Stop = I_Start + 3
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 4
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_VI4_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_VI8_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        INTEGER(8), DIMENSION(:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, N_Row, I_Start, I_Stop

!- - - - - - - - - - - - INSERT INTEGER(8) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )

        I_Start = Ini
        DO I = 1, N_Row
        I_Stop = I_Start + 7
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 8
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_VI8_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_VR4_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        REAL(4), DIMENSION(:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, N_Row, I_Start, I_Stop

!- - - - - - - - - - - - INSERT REAL(4) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )

        I_Start = Ini
        DO I = 1, N_Row
        I_Stop = I_Start + 3
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 4
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_VR4_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_VR8_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        REAL(8), DIMENSION(:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, N_Row, I_Start, I_Stop

!- - - - - - - - - - - - INSERT REAL(8) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )

        I_Start = Ini
        DO I = 1, N_Row
        I_Stop = I_Start + 7
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 8
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_VR8_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_VR16_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        REAL(16), DIMENSION(:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, N_Row, I_Start, I_Stop

!- - - - - - - - - - - - INSERT REAL(16) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )

        I_Start = Ini
        DO I = 1, N_Row
        I_Stop = I_Start + 15
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 16
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_VR16_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_VC4_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        COMPLEX(4), DIMENSION(:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, N_Row, I_Start, I_Stop

!- - - - - - - - - - - - INSERT COMPLEX(4) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )

        I_Start = Ini
        DO I = 1, N_Row
        I_Stop = I_Start + 7
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 8
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_VC4_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_VC8_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        COMPLEX(8), DIMENSION(:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, N_Row, I_Start, I_Stop

!- - - - - - - - - - - - INSERT COMPLEX(8) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )

        I_Start = Ini
        DO I = 1, N_Row
        I_Stop = I_Start + 15
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 16
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_VC8_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_VC16_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        COMPLEX(16), DIMENSION(:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, N_Row, I_Start, I_Stop

!- - - - - - - - - - - - INSERT COMPLEX(16) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )

        I_Start = Ini
        DO I = 1, N_Row
        I_Stop = I_Start + 31
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 32
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_VC16_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_MI1_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        INTEGER(1), DIMENSION(:,:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, J, N_Row, N_Col, I_Start

!- - - - - - - - - - - - INSERT INTEGER(1) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )
        N_Col = SIZE( New_Value, 2 )

        I_Start = Ini
        DO J = 1, N_Col
        DO I = 1, N_Row
        This%Value( I_Start ) = Transfer( New_Value( I, J ), This%Value( I_Start ) )
        I_Start = I_Start + 1
        END DO
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_MI1_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_MI2_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        INTEGER(2), DIMENSION(:,:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, J, N_Row, N_Col, I_Start, I_Stop

!- - - - - - - - - - - - INSERT INTEGER(2) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )
        N_Col = SIZE( New_Value, 2 )

        I_Start = Ini
        DO J = 1, N_Col
        DO I = 1, N_Row
        I_Stop = I_Start + 1
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I, J ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 2
        END DO
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_MI2_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_MI4_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        INTEGER(4), DIMENSION(:,:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, J, N_Row, N_Col, I_Start, I_Stop

!- - - - - - - - - - - - INSERT INTEGER(4) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )
        N_Col = SIZE( New_Value, 2 )

        I_Start = Ini
        DO J = 1, N_Col
        DO I = 1, N_Row
        I_Stop = I_Start + 3
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I, J ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 4
        END DO
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_MI4_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_MI8_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        INTEGER(8), DIMENSION(:,:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, J, N_Row, N_Col, I_Start, I_Stop

!- - - - - - - - - - - - INSERT INTEGER(8) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )
        N_Col = SIZE( New_Value, 2 )

        I_Start = Ini
        DO J = 1, N_Col
        DO I = 1, N_Row
        I_Stop = I_Start + 7
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I, J ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 8
        END DO
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_MI8_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_MR4_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        REAL(4), DIMENSION(:,:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, J, N_Row, N_Col, I_Start, I_Stop

!- - - - - - - - - - - - INSERT REAL(4) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )
        N_Col = SIZE( New_Value, 2 )

        I_Start = Ini
        DO J = 1, N_Col
        DO I = 1, N_Row
        I_Stop = I_Start + 3
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I, J ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 4
        END DO
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_MR4_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_MR8_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        REAL(8), DIMENSION(:,:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, J, N_Row, N_Col, I_Start, I_Stop

!- - - - - - - - - - - - INSERT REAL(8) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )
        N_Col = SIZE( New_Value, 2 )

        I_Start = Ini
        DO J = 1, N_Col
        DO I = 1, N_Row
        I_Stop = I_Start + 7
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I, J ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 8
        END DO
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_MR8_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_MR16_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        REAL(16), DIMENSION(:,:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, J, N_Row, N_Col, I_Start, I_Stop

!- - - - - - - - - - - - INSERT REAL(16) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )
        N_Col = SIZE( New_Value, 2 )

        I_Start = Ini
        DO J = 1, N_Col
        DO I = 1, N_Row
        I_Stop = I_Start + 15
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I, J ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 16
        END DO
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_MR16_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_MC4_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        COMPLEX(4), DIMENSION(:,:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, J, N_Row, N_Col, I_Start, I_Stop

!- - - - - - - - - - - - INSERT COMPLEX(4) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )
        N_Col = SIZE( New_Value, 2 )

        I_Start = Ini
        DO J = 1, N_Col
        DO I = 1, N_Row
        I_Stop = I_Start + 7
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I, J ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 8
        END DO
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_MC4_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_MC8_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        COMPLEX(8), DIMENSION(:,:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, J, N_Row, N_Col, I_Start, I_Stop

!- - - - - - - - - - - - INSERT COMPLEX(8) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )
        N_Col = SIZE( New_Value, 2 )

        I_Start = Ini
        DO J = 1, N_Col
        DO I = 1, N_Row
        I_Stop = I_Start + 15
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I, J ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 16
        END DO
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_MC8_DATA
!******************************************************************************
 
!******************************************************************************
        SUBROUTINE INSERT_MC16_DATA( THIS, Ini, New_Value )
!******************************************************************************
        IMPLICIT NONE
        CLASS( G_LINK ) :: THIS
        INTEGER, INTENT( IN ) :: Ini
        COMPLEX(16), DIMENSION(:,:), INTENT( IN ) :: New_Value
 
        INTEGER :: I, J, N_Row, N_Col, I_Start, I_Stop

!- - - - - - - - - - - - INSERT COMPLEX(16) to LINK  - - - - - - - - - - - - - -
        N_Row = SIZE( New_Value, 1 )
        N_Col = SIZE( New_Value, 2 )

        I_Start = Ini
        DO J = 1, N_Col
        DO I = 1, N_Row
        I_Stop = I_Start + 31
        This%Value(  I_Start: I_Stop ) = Transfer( New_Value( I, J ), This%Value(  I_Start: I_Stop ) )
        I_Start = I_Start + 32
        END DO
        END DO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
         
!******************************************************************************
        END SUBROUTINE INSERT_MC16_DATA
!******************************************************************************
 
!******************************************************************************
        FUNCTION GIVE_KEY(THIS)
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        CHARACTER(:), POINTER :: GIVE_KEY
        INTEGER :: Key_Len, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_KEY'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Key_Len = TRANSFER( This%Value( 1: Int_Byte ), Key_Len )

        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        GIVE_KEY => Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        ELSE
        GIVE_KEY => Null_Key
        END IF

!******************************************************************************
        END FUNCTION GIVE_KEY
!==============================================================================
 
!******************************************************************************
        Function GIVE_SS_Data(THIS )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        CHARACTER(:), POINTER :: Give_SS_Data
        INTEGER :: N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_SS_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

        Ini = Data_Loc 
        Fnl = Data_Loc + N_Row * N_Col - 1
        Give_SS_Data => Cast_to_STRING( Fnl - Ini + 1, This%Value( Ini: Fnl)  )

        ELSE
        GIVE_SS_DATA => Null_Key
        END IF

!******************************************************************************
        END Function GIVE_SS_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_SI1_Data(THIS, I, J, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER, INTENT( IN ) :: I, J
        INTEGER(I1P), INTENT( IN OUT ) :: Val
        INTEGER :: N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_SI1_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

        Fnl = Data_Loc + ( ( J - 1 ) * N_Row + I - 1 )
        Val = This%Value( Fnl )

        ELSE
        Val = 0_I1P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_SI1_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_VI1_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER(I1P), DIMENSION(:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VI1_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Fnl= Data_Loc 
         DO J = 1, N_Col
         DO I = 1, N_Row
 
         Val( (J - 1)*N_Row + I ) = This%Value( Fnl )
         Fnl = Fnl + 1
         END DO
         END DO
         

        ELSE
        Val = 0_I1P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_VI1_Data
!==============================================================================
 
!******************************************************************************
        SUBROUTINE GIVE_MI1_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER(I1P), DIMENSION(:,:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VI1_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Fnl= Data_Loc 
         DO J = 1, N_Col
         DO I = 1, N_Row
 
         Val( I, J ) = This%Value( Fnl )
         Fnl = Fnl + 1
         END DO
         END DO
         

        ELSE
        Val = 0_I1P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_MI1_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_SI2_Data(THIS, I, J, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER, INTENT( IN ) :: I, J
        INTEGER(I2P), INTENT( IN OUT ) :: Val
        INTEGER :: N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_SI2_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

        Ini = Data_Loc + ( ( J - 1 ) * N_Row + I - 1 ) * 2
        Fnl = Ini + 1
        Val = Cast_to_I2( This%Value( Ini: Fnl)  )

        ELSE
        Val = 0_I2P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_SI2_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_VI2_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER(I2P), DIMENSION(:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VI2_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 1
 
         Val( (J - 1)*N_Row + I ) = Cast_to_I2( This%Value( Ini: Fnl)  )
         Ini = Ini + 2
         END DO
         END DO

        ELSE
        Val = 0_I2P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_VI2_Data
!==============================================================================
 
!******************************************************************************
        SUBROUTINE GIVE_MI2_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER(I2P), DIMENSION(:,:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VI2_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 1
 
         Val( I, J ) = Cast_to_I2( This%Value( Ini: Fnl)  )
         Ini = Ini + 2
         END DO
         END DO

        ELSE
        Val = 0_I2P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_MI2_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_SI4_Data(THIS, I, J, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER, INTENT( IN ) :: I, J
        INTEGER(I4P), INTENT( IN OUT ) :: Val
        INTEGER :: N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_SI4_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

        Ini = Data_Loc + ( ( J - 1 ) * N_Row + I - 1 ) * 4
        Fnl = Ini + 3
        Val = Cast_to_I4( This%Value( Ini: Fnl)  )

        ELSE
        Val = 0_I4P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_SI4_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_VI4_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER(I4P), DIMENSION(:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VI4_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 3
 
         Val( (J - 1)*N_Row + I ) = Cast_to_I4( This%Value( Ini: Fnl)  )
         Ini = Ini + 4
         END DO
         END DO

        ELSE
        Val = 0_I4P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_VI4_Data
!==============================================================================
 
!******************************************************************************
        SUBROUTINE GIVE_MI4_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER(I4P), DIMENSION(:,:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VI4_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 3
 
         Val( I, J ) = Cast_to_I4( This%Value( Ini: Fnl)  )
         Ini = Ini + 4
         END DO
         END DO

        ELSE
        Val = 0_I4P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_MI4_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_SI8_Data(THIS, I, J, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER, INTENT( IN ) :: I, J
        INTEGER(I8P), INTENT( IN OUT ) :: Val
        INTEGER :: N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_SI8_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

        Ini = Data_Loc + ( ( J - 1 ) * N_Row + I - 1 ) * 8
        Fnl = Ini + 7
        Val = Cast_to_I8( This%Value( Ini: Fnl)  )

        ELSE
        Val = 0_I8P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_SI8_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_VI8_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER(I8P), DIMENSION(:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VI8_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 7
 
         Val( (J - 1)*N_Row + I ) = Cast_to_I8( This%Value( Ini: Fnl)  )
         Ini = Ini + 8
         END DO
         END DO

        ELSE
        Val = 0_I8P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_VI8_Data
!==============================================================================
 
!******************************************************************************
        SUBROUTINE GIVE_MI8_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER(I8P), DIMENSION(:,:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VI8_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 7
 
         Val( I, J ) = Cast_to_I8( This%Value( Ini: Fnl)  )
         Ini = Ini + 8
         END DO
         END DO

        ELSE
        Val = 0_I8P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_MI8_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_SR4_Data(THIS, I, J, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER, INTENT( IN ) :: I, J
        REAL(R4P), INTENT( IN OUT ) :: Val
        INTEGER :: N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_SR4_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

        Ini = Data_Loc + ( ( J - 1 ) * N_Row + I - 1 ) * 4
        Fnl = Ini + 3
        Val = Cast_to_R4( This%Value( Ini: Fnl)  )

        ELSE
        Val = 0.0_R4P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_SR4_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_VR4_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        REAL(R4P), DIMENSION(:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VR4_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 3
 
         Val( (J - 1)*N_Row + I ) = Cast_to_R4( This%Value( Ini: Fnl)  )
         Ini = Ini + 4
         END DO
         END DO

        ELSE
        Val = 0.0_R4P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_VR4_Data
!==============================================================================
 
!******************************************************************************
        SUBROUTINE GIVE_MR4_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        REAL(R4P), DIMENSION(:,:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VR4_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 3
 
         Val( I, J ) = Cast_to_R4( This%Value( Ini: Fnl)  )
         Ini = Ini + 4
         END DO
         END DO

        ELSE
        Val = 0.0_R4P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_MR4_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_SR8_Data(THIS, I, J, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER, INTENT( IN ) :: I, J
        REAL(R8P), INTENT( IN OUT ) :: Val
        INTEGER :: N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_SR8_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

        Ini = Data_Loc + ( ( J - 1 ) * N_Row + I - 1 ) * 8
        Fnl = Ini + 7
        Val = Cast_to_R8( This%Value( Ini: Fnl)  )

        ELSE
        Val = 0.0_R8P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_SR8_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_VR8_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        REAL(R8P), DIMENSION(:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VR8_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 7
 
         Val( (J - 1)*N_Row + I ) = Cast_to_R8( This%Value( Ini: Fnl)  )
         Ini = Ini + 8
         END DO
         END DO

        ELSE
        Val = 0.0_R8P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_VR8_Data
!==============================================================================
 
!******************************************************************************
        SUBROUTINE GIVE_MR8_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        REAL(R8P), DIMENSION(:,:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VR8_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 7
 
         Val( I, J ) = Cast_to_R8( This%Value( Ini: Fnl)  )
         Ini = Ini + 8
         END DO
         END DO

        ELSE
        Val = 0.0_R8P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_MR8_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_SR16_Data(THIS, I, J, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER, INTENT( IN ) :: I, J
        REAL(R16P), INTENT( IN OUT ) :: Val
        INTEGER :: N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_SR16_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

        Ini = Data_Loc + ( ( J - 1 ) * N_Row + I - 1 ) * 16
        Fnl = Ini + 15
        Val = Cast_to_R16( This%Value( Ini: Fnl)  )

        ELSE
        Val = 0.0_R16P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_SR16_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_VR16_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        REAL(R16P), DIMENSION(:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VR16_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 15
 
         Val( (J - 1)*N_Row + I ) = Cast_to_R16( This%Value( Ini: Fnl)  )
         Ini = Ini + 16
         END DO
         END DO

        ELSE
        Val = 0.0_R16P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_VR16_Data
!==============================================================================
 
!******************************************************************************
        SUBROUTINE GIVE_MR16_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        REAL(R16P), DIMENSION(:,:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VR16_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 15
 
         Val( I, J ) = Cast_to_R16( This%Value( Ini: Fnl)  )
         Ini = Ini + 16
         END DO
         END DO

        ELSE
        Val = 0.0_R16P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_MR16_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_SC4_Data(THIS, I, J, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER, INTENT( IN ) :: I, J
        COMPLEX(R4P), INTENT( IN OUT ) :: Val
        INTEGER :: N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_SC4_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

        Ini = Data_Loc + ( ( J - 1 ) * N_Row + I - 1 ) * 8
        Fnl = Ini + 7
        Val = Cast_to_C4( This%Value( Ini: Fnl)  )

        ELSE
        Val = 0.0_R4P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_SC4_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_VC4_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        COMPLEX(R4P), DIMENSION(:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VC4_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 7
 
         Val( (J - 1)*N_Row + I ) = Cast_to_C4( This%Value( Ini: Fnl)  )
         Ini = Ini + 8
         END DO
         END DO

        ELSE
        Val = 0.0_R4P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_VC4_Data
!==============================================================================
 
!******************************************************************************
        SUBROUTINE GIVE_MC4_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        COMPLEX(R4P), DIMENSION(:,:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VC4_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 7
 
         Val( I, J ) = Cast_to_C4( This%Value( Ini: Fnl)  )
         Ini = Ini + 8
         END DO
         END DO

        ELSE
        Val = 0.0_R4P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_MC4_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_SC8_Data(THIS, I, J, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER, INTENT( IN ) :: I, J
        COMPLEX(R8P), INTENT( IN OUT ) :: Val
        INTEGER :: N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_SC8_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

        Ini = Data_Loc + ( ( J - 1 ) * N_Row + I - 1 ) * 16
        Fnl = Ini + 15
        Val = Cast_to_C8( This%Value( Ini: Fnl)  )

        ELSE
        Val = 0.0_R8P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_SC8_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_VC8_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        COMPLEX(R8P), DIMENSION(:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VC8_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 15
 
         Val( (J - 1)*N_Row + I ) = Cast_to_C8( This%Value( Ini: Fnl)  )
         Ini = Ini + 16
         END DO
         END DO

        ELSE
        Val = 0.0_R8P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_VC8_Data
!==============================================================================
 
!******************************************************************************
        SUBROUTINE GIVE_MC8_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        COMPLEX(R8P), DIMENSION(:,:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VC8_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 15
 
         Val( I, J ) = Cast_to_C8( This%Value( Ini: Fnl)  )
         Ini = Ini + 16
         END DO
         END DO

        ELSE
        Val = 0.0_R8P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_MC8_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_SC16_Data(THIS, I, J, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER, INTENT( IN ) :: I, J
        COMPLEX(R16P), INTENT( IN OUT ) :: Val
        INTEGER :: N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_SC16_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

        Ini = Data_Loc + ( ( J - 1 ) * N_Row + I - 1 ) * 32
        Fnl = Ini + 31
        Val = Cast_to_C16( This%Value( Ini: Fnl)  )

        ELSE
        Val = 0.0_R16P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_SC16_Data
!==============================================================================

!******************************************************************************
        SUBROUTINE GIVE_VC16_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        COMPLEX(R16P), DIMENSION(:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VC16_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 31
 
         Val( (J - 1)*N_Row + I ) = Cast_to_C16( This%Value( Ini: Fnl)  )
         Ini = Ini + 32
         END DO
         END DO

        ELSE
        Val = 0.0_R16P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_VC16_Data
!==============================================================================
 
!******************************************************************************
        SUBROUTINE GIVE_MC16_Data(THIS, Val )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        COMPLEX(R16P), DIMENSION(:,:), INTENT( IN OUT ) :: Val
        INTEGER :: I, J, N_Row, N_Col
        INTEGER :: Data_Loc, Ini, Fnl

        PROCEDURE_NAME = "'GIVE_VC16_Data'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row ) 

        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col ) 

        Ini = 4 * Int_Byte + 1
        Fnl = 5 * Int_Byte
        Data_Loc = TRANSFER( This%Value( Ini: Fnl ), Data_Loc )

         Ini = Data_Loc
         DO J = 1, N_Col
         DO I = 1, N_Row
         Fnl = Ini + 31
 
         Val( I, J ) = Cast_to_C16( This%Value( Ini: Fnl)  )
         Ini = Ini + 32
         END DO
         END DO

        ELSE
        Val = 0.0_R16P
        END IF

!******************************************************************************
        END SUBROUTINE GIVE_MC16_Data
!==============================================================================

 
!******************************************************************************
        FUNCTION Link_Data_Type(THIS)
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        CHARACTER(4) :: Link_Data_Type
        INTEGER :: Data_t
 
        IF( ALLOCATED( THIS%Value ) )THEN

        Data_t = TRANSFER( This%Value( 1 + Int_Byte: 2 * Int_Byte ), Data_t )

         SELECT CASE( Data_t )
         CASE( 0 )
           Link_Data_Type = "CHAR"

         CASE( 1 )
           Link_Data_Type = "I_01"

         CASE( 2 )
           Link_Data_Type = "I_02"

         CASE( 3 )
           Link_Data_Type = "I_04"

         CASE( 4 )
           Link_Data_Type = "I_08"

         CASE( 11 )
           Link_Data_Type = "R_04"

         CASE( 12 )
           Link_Data_Type = "R_08"

         CASE( 13 )
           Link_Data_Type = "R_16"

         CASE( 21 )
           Link_Data_Type = "C_04"

         CASE( 22 )
           Link_Data_Type = "C_08"

         CASE( 23 )
           Link_Data_Type = "C_16"

         CASE DEFAULT
           Link_Data_Type = "XXXX"
         END SELECT           

        ELSE

           Link_Data_Type = "0000"

        END IF
 
!******************************************************************************
        END FUNCTION Link_Data_Type
!==============================================================================
 
!******************************************************************************
        FUNCTION GIVE_DATA_SIZE_OF_LINK( THIS, DIR ) RESULT( VAL )
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER, INTENT( IN ) :: DIR
        INTEGER :: VAL
        INTEGER :: N_Row, N_Col
        INTEGER :: Ini, Fnl
 
        PROCEDURE_NAME = "'GIVE_SIZE_OF_LINK'"

        IF( ALLOCATED( THIS%Value ) )THEN

        Ini = 2 * Int_Byte + 1
        Fnl = 3 * Int_Byte
        N_Row = TRANSFER( This%Value( Ini: Fnl ), N_Row )
        Ini = 3 * Int_Byte + 1
        Fnl = 4 * Int_Byte
        N_Col = TRANSFER( This%Value( Ini: Fnl ), N_Col )

            SELECT CASE ( DIR )
            CASE( 1 )
             VAL = N_Row
            CASE( 2 )
             VAL = N_Col
            CASE DEFAULT
             VAL = N_Row * N_Col
            END SELECT
        
        ELSE

      LOCAL_FLAG="*** NO VALUE IS STORED IN '"//THIS%GIVE_KEY()//"' ***"
      WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )
      WRITE(*,'(A)')BTRIM( LOCAL_FLAG )
      STOP
         ENDIF
 
!******************************************************************************
        END FUNCTION GIVE_DATA_SIZE_OF_LINK
!==============================================================================
 
!******************************************************************************
        SUBROUTINE PRINT_LINK_VALUE(THIS, IFILE)
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        INTEGER, OPTIONAL, INTENT( IN ) :: IFILE
 
        INTEGER :: N_Row, N_Col, Key_Len, Data_t
        INTEGER :: I, J, Ini_Data, Ini, Fnl

       PROCEDURE_NAME = "'PRINT_LINK_VALUE'"
 
        SELECT CASE( ALLOCATED( THIS%Value ) )
        CASE( .FALSE. )
 
             IF(PRESENT(IFILE))THEN
        WRITE(IFILE,'("*** Link Does Not Exist ***",A)')
             ELSE
        WRITE(*,'("*** Link Does Not Exist ***",A)')
             END IF
 
        CASE DEFAULT

        CALL THIS%Give_CONTENT( Key_Len, Data_t, N_Row, N_Col, Ini_Data )
 
!--------------- SELECT SCALAR/VECTOR/MATRIX VALUES --------------------
 
           SELECT CASE( Data_t )
           CASE( 0 )
             IF(PRESENT(IFILE))THEN
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(IFILE,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(IFILE,'("Stored Data_Type: CHARACTER(LEN=",I4,")")') Size(This%Value, 1) - Header_Len - Key_Len
        Ini = Ini_Data
        Fnl = Size( This%Value,1)
        WRITE(IFILE,'("Stored String: ")')
        WRITE(IFILE,*) Cast_to_String( Fnl-Ini + 1, THIS%Value( Ini: Fnl ) )
             ELSE
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(*,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(*,'("Stored Data_Type: CHARACTER(LEN=",I4,")")') Size(This%Value, 1) - Header_Len - Key_Len
        Ini = Ini_Data
        Fnl = Size( This%Value,1)
        WRITE(*,'("Stored String: ")')
        WRITE(*,*) Cast_to_String( Fnl-Ini + 1, THIS%Value( Ini: Fnl ) )
             ENDIF
 
           CASE( 1 )
             IF(PRESENT(IFILE))THEN
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(IFILE,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(IFILE,'("Stored Data_Type: INTEGER(1)")')
        WRITE(IFILE,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(IFILE,'("Stored Data: ")')
        Fnl = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
        WRITE(IFILE,'(2X,I4)', ADVANCE="NO")  THIS%Value( Fnl )
         Fnl = Fnl + 1
          END DO
        WRITE(IFILE,*)
         END DO
             ELSE
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(*,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(*,'("Stored Data_Type: INTEGER(1)")')
        WRITE(*,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(*,'("Stored Data: ")')
        Fnl = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
        WRITE(*,'(2X,I4)', ADVANCE="NO") THIS%Value( Fnl )
         Fnl = Fnl + 1
          END DO
        WRITE(*,*)
         END DO
             ENDIF
 
           CASE( 2 )
             IF(PRESENT(IFILE))THEN
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(IFILE,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(IFILE,'("Stored Data_Type: INTEGER(2)")')
        WRITE(IFILE,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(IFILE,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 1 
        WRITE(IFILE,'(2X,I4)', ADVANCE="NO") Cast_to_I2( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 2
          END DO
        WRITE(IFILE,*)
         END DO
             ELSE
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(*,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(*,'("Stored Data_Type: INTEGER(2)")')
        WRITE(*,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(*,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 1 
        WRITE(*,'(2X,I4)', ADVANCE="NO") Cast_to_I2( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 2
          END DO
        WRITE(*,*)
         END DO
             ENDIF
 
           CASE( 3 )
             IF(PRESENT(IFILE))THEN
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(IFILE,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(IFILE,'("Stored Data_Type: INTEGER(4)")')
        WRITE(IFILE,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(IFILE,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 3 
        WRITE(IFILE,'(2X,I4)', ADVANCE="NO") Cast_to_I4( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 4
          END DO
        WRITE(IFILE,*)
         END DO
             ELSE
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(*,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(*,'("Stored Data_Type: INTEGER(4)")')
        WRITE(*,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(*,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 3
        WRITE(*,'(2X,I4)', ADVANCE="NO") Cast_to_I4( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 4
          END DO
        WRITE(*,*)
         END DO
             ENDIF
 
           CASE( 4 )
             IF(PRESENT(IFILE))THEN
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(IFILE,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(IFILE,'("Stored Data_Type: INTEGER(8)")')
        WRITE(IFILE,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(IFILE,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 7
        WRITE(IFILE,'(2X,I8)', ADVANCE="NO") Cast_to_I8( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 8
          END DO
        WRITE(IFILE,*)
         END DO
             ELSE
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(*,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(*,'("Stored Data_Type: INTEGER(8)")')
        WRITE(*,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(*,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 7
        WRITE(*,'(2X,I8)', ADVANCE="NO") Cast_to_I8( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 8
          END DO
        WRITE(*,*)
         END DO
             ENDIF
 
           CASE( 11 )
             IF(PRESENT(IFILE))THEN
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(IFILE,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(IFILE,'("Stored Data_Type: REAL(4)")')
        WRITE(IFILE,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(IFILE,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 3
        WRITE(IFILE,'(2X,I4)', ADVANCE="NO") Cast_to_R4( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 4
          END DO
        WRITE(IFILE,*)
         END DO
             ELSE
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(*,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(*,'("Stored Data_Type: REAL(4)")')
        WRITE(*,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(*,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 3
        WRITE(*,'(2X,I4)', ADVANCE="NO") Cast_to_R4( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 4
          END DO
        WRITE(*,*)
         END DO
             ENDIF
 
           CASE( 12 )
             IF(PRESENT(IFILE))THEN
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(IFILE,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(IFILE,'("Stored Data_Type: REAL(8)")')
        WRITE(IFILE,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(IFILE,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 7
        WRITE(IFILE,'(2X,G16.8)', ADVANCE="NO") Cast_to_R8( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 8
          END DO
        WRITE(IFILE,*)
         END DO
             ELSE
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(*,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(*,'("Stored Data_Type: REAL(8)")')
        WRITE(*,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(*,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 7
        WRITE(*,'(2X,G16.8)', ADVANCE="NO") Cast_to_R8( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 8
          END DO
        WRITE(*,*)
         END DO
             ENDIF
 
           CASE( 13 )
             IF(PRESENT(IFILE))THEN
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(IFILE,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(IFILE,'("Stored Data_Type: REAL(16)")')
        WRITE(IFILE,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(IFILE,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 15
        WRITE(IFILE,'(2X,G16.8)', ADVANCE="NO") Cast_to_R16( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 16
          END DO
        WRITE(IFILE,*)
         END DO
             ELSE
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(*,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(*,'("Stored Data_Type: REAL(16)")')
        WRITE(*,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(*,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 15
        WRITE(*,'(2X,G16.8)', ADVANCE="NO") Cast_to_R16( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 16
          END DO
        WRITE(*,*)
         END DO
             ENDIF
 
           CASE( 21 )
             IF(PRESENT(IFILE))THEN
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(IFILE,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(IFILE,'("Stored Data_Type: COMPLEX(4)")')
        WRITE(IFILE,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(IFILE,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 7
        WRITE(IFILE,'(2X,I4)', ADVANCE="NO") Cast_to_C4( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 8
          END DO
        WRITE(IFILE,*)
         END DO
             ELSE
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(*,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(*,'("Stored Data_Type: COMPLEX(4)")')
        WRITE(*,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(*,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 7
        WRITE(*,'(2X,I4)', ADVANCE="NO") Cast_to_C4( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 8
          END DO
        WRITE(*,*)
         END DO
             ENDIF
 
           CASE( 22 )
             IF(PRESENT(IFILE))THEN
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(IFILE,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(IFILE,'("Stored Data_Type: COMPLEX(8)")')
        WRITE(IFILE,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(IFILE,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 15
        WRITE(IFILE,'(2X,G16.8)', ADVANCE="NO") Cast_to_C8( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 16
          END DO
        WRITE(IFILE,*)
         END DO
             ELSE
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(*,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(*,'("Stored Data_Type: COMPLEX(8)")')
        WRITE(*,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(*,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 15
        WRITE(*,'(2X,"(",G16.8,4x,G16.8,")")', ADVANCE="NO") Cast_to_C8( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 16
          END DO
        WRITE(*,*)
         END DO
             ENDIF
 
           CASE( 23 )
             IF(PRESENT(IFILE))THEN
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(IFILE,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(IFILE,'("Stored Data_Type: COMPLEX(16)")')
        WRITE(IFILE,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(IFILE,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 31
        WRITE(IFILE,'(2X,I4)', ADVANCE="NO") Cast_to_C16( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 32
          END DO
        WRITE(IFILE,*)
         END DO
             ELSE
        Ini = Header_Len + 1
        Fnl = Header_Len + Key_Len
        WRITE(*,'("Label: ",A)') Cast_to_String( Key_Len, THIS%Value( Ini: Fnl ) )
        WRITE(*,'("Stored Data_Type: COMPLEX(16)")')
        WRITE(*,'("Data Block Size : ",I8,"X",I8)')N_Row, N_Col
        WRITE(*,'("Stored Data: ")')
        Ini = Ini_Data
         DO J = 1, N_Col
          DO I = 1, N_Row
         Fnl=  Ini + 31
        WRITE(*,'(2X,I4)', ADVANCE="NO") Cast_to_C16( THIS%Value( Ini: Fnl ) )
         Ini = Ini + 32
          END DO
        WRITE(*,*)
         END DO
             ENDIF
 
           CASE DEFAULT
             IF(PRESENT(IFILE))THEN
        WRITE(IFILE,'("*** No Value is Stored In This Link ***",A)')
             ELSE
        WRITE(*,'("*** No Value is Stored In This Link ***",A)')
             END IF
 
           END SELECT
!----------------------------------------------------------------------
 
        END SELECT
 
 
!******************************************************************************
        END SUBROUTINE PRINT_LINK_VALUE
!==============================================================================
 
!******************************************************************************
        FUNCTION GIVE_NEXT(THIS)
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        TYPE(G_LINK), POINTER :: GIVE_NEXT
 
          GIVE_NEXT => THIS%NEXT
 
!******************************************************************************
        END FUNCTION GIVE_NEXT
!==============================================================================
 
!******************************************************************************
        SUBROUTINE SET_NEXT(THIS, NEXT)
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
        TYPE(G_LINK), POINTER, INTENT( IN ) :: NEXT
 
          THIS%NEXT => NEXT
 
!******************************************************************************
        END SUBROUTINE SET_NEXT
!==============================================================================
 
!******************************************************************************
        SUBROUTINE SET_NEXT_NULL(THIS)
!******************************************************************************
        IMPLICIT NONE
        CLASS(G_LINK) :: THIS
 
          THIS%NEXT => NULL()
 
!******************************************************************************
        END SUBROUTINE SET_NEXT_NULL
!==============================================================================
 
!******************************************************************************
        SUBROUTINE ERASE_LINK(THIS)
!******************************************************************************
        IMPLICIT NONE
          CLASS(G_LINK) :: THIS
 
         IF( ASSOCIATED( THIS%NEXT ) ) NULLIFY( THIS%NEXT )
         IF( ALLOCATED( THIS%Value ) ) DEALLOCATE( THIS%Value )
 
!******************************************************************************
        END SUBROUTINE ERASE_LINK
!==============================================================================
 
 
!----------------------------[ LINK FUNCTIONS ARE ABOVE ]---------------------------------
 
 
!----------------------------[ LIST FUNCTIONS ARE DEFINED HERE ]--------------------------

!******************************************************************************
         SUBROUTINE SET_LIST_TYPE(THIS, Data_t )
!******************************************************************************
         IMPLICIT NONE
         CLASS(G_LIST) :: THIS
         CLASS(*), INTENT( IN ) :: Data_t

         PROCEDURE_NAME = "'Set_List_Type'"

         CALL THIS%ERASE() 

         SELECT TYPE ( Data_t )
         TYPE IS ( CHARACTER(*) )
         THIS%Data_t = 0 
         TYPE IS ( INTEGER(I1P) )
         THIS%Data_t = 1 
         TYPE IS ( INTEGER(I2P) )
         THIS%Data_t = 2 
         TYPE IS ( INTEGER(I4P) )
         THIS%Data_t = 3 
         TYPE IS ( INTEGER(I8P) )
         THIS%Data_t = 5 
         TYPE IS ( REAL(R4P) )
         THIS%Data_t = 11 
         TYPE IS ( REAL(R8P) )
         THIS%Data_t = 12 
         TYPE IS ( REAL(R16P) )
         THIS%Data_t = 13 
         TYPE IS ( COMPLEX(R4P) )
         THIS%Data_t = 21 
         TYPE IS ( COMPLEX(R8P) )
         THIS%Data_t = 22 
         TYPE IS ( COMPLEX(R16P) )
         THIS%Data_t = 23 
         CLASS DEFAULT
         THIS%Data_t = - 1
         END SELECT
 
!******************************************************************************
         END SUBROUTINE SET_LIST_TYPE
!==============================================================================
 
!******************************************************************************
         SUBROUTINE PRINT_VALUE(THIS, KEY, IFILE)
!******************************************************************************
         IMPLICIT NONE
           CLASS(G_LIST) :: THIS
         CHARACTER(LEN=*), INTENT( IN ) :: KEY
        INTEGER, OPTIONAL, INTENT( IN ) :: IFILE
 
         LOGICAL(LGT) :: IS_IN_LIST
         TYPE(G_LINK), POINTER :: CURR

       PROCEDURE_NAME = "'PRINT_VALUE'"
 
         IS_IN_LIST = .FALSE.
           IF (.NOT.THIS%IS_EMPTY()) THEN
 
         CURR => THIS%FIRST
!---------------[ SEARCHING FOR THE LINK CONTAINING 'KEY' ]---------------------
         LOOP_SEARCH: DO WHILE(ASSOCIATED(CURR))
 
            IF( STRCMP( CURR%Give_KEY(), KEY) )THEN
             IS_IN_LIST = .TRUE.
             EXIT LOOP_SEARCH
            ELSE
           CURR => CURR%NEXT
            ENDIF
         END DO LOOP_SEARCH
!-------------------------------------------------------------------------------
 
                 IF(IS_IN_LIST)THEN
            IF(PRESENT(IFILE))THEN
         CALL CURR%PRINT_VALUE(IFILE)
            ELSE
         CALL CURR%PRINT_VALUE()
            ENDIF
 
                 ELSE
           PRINT*, "LIST DOES NOT HAVE  KEY :: '"//BTRIM(KEY)//"'"
                 ENDIF
 
           ENDIF
!******************************************************************************
         END SUBROUTINE PRINT_VALUE
!==============================================================================
 
!******************************************************************************
         SUBROUTINE PRINT_LIST(THIS, IFILE)
!******************************************************************************
         IMPLICIT NONE
           CLASS(G_LIST) :: THIS
         TYPE(G_LINK), POINTER :: CURR
        INTEGER, OPTIONAL, INTENT( IN ) :: IFILE

       PROCEDURE_NAME = "'PRINT_LIST'"
 
           IF (.NOT.THIS%IS_EMPTY()) THEN
 
         CURR => THIS%FIRST
         DO WHILE(ASSOCIATED(CURR))
            IF(PRESENT(IFILE))THEN
         CALL CURR%PRINT_VALUE(IFILE)
            ELSE
         CALL CURR%PRINT_VALUE()
            ENDIF
           CURR => CURR%NEXT
         END DO
 
           PRINT*, "NO MORE LINK IS PRESENT!!"
           ELSE
           PRINT*, "LIST DOESNOT EXISTS!!"
 
           ENDIF
!******************************************************************************
         END SUBROUTINE PRINT_LIST
!==============================================================================
 
!******************************************************************************
         SUBROUTINE PUSH_FRONT_S(THIS, NEW_KEY, NEW_VALUE)
!******************************************************************************
          IMPLICIT NONE
          CLASS(G_LIST) :: THIS
          CHARACTER(LEN=*), INTENT( IN ) :: NEW_KEY
          CLASS(*), INTENT( IN ) :: NEW_VALUE
 
          TYPE(G_LINK), POINTER :: NEW_LINK
          INTEGER :: Key_Len, Data_Type, N_Row, N_Col, Ini_Data

          PROCEDURE_NAME = "'PUSH_FRONT_S'"

          IF( ASSOCIATED( New_Link ) ) NULLIFY( New_link )


          SELECT TYPE( New_Value )
          TYPE IS ( CHARACTER(*) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 0, LEN_BTRIM(NEW_VALUE), 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SS_DATA( Ini_Data, BTRIM( New_Value ) )

          TYPE IS ( INTEGER(1) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 1, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI1_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(2) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 2, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI2_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 3, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI4_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 4, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 11, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SR4_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY),12, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SR8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 13, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SR16_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 21, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SC4_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 22, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SC8_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 23, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SC16_DATA( Ini_Data, New_Value )

          CLASS DEFAULT
       LOCAL_FLAG = "UNKNOWN DATA_TYPE SUPPLIED VALUE ="//INT_TO_STR(DATA_TYPE)
       WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )//" :: "
       WRITE(*,'(A)')BTRIM( LOCAL_FLAG )
          END SELECT

 
           IF (.NOT. ASSOCIATED(THIS%FIRST)) THEN
              THIS%FIRST => New_Link
              THIS%LAST => THIS%FIRST
           ELSE
             New_Link%Next => This%First
             THIS%FIRST => New_Link
           ENDIF
 
!******************************************************************************
         END SUBROUTINE PUSH_FRONT_S
!==============================================================================
 
!******************************************************************************
         SUBROUTINE PUSH_FRONT_V(THIS, NEW_KEY, NEW_VALUE)
!******************************************************************************
          IMPLICIT NONE
          CLASS(G_LIST) :: THIS
          CHARACTER(LEN=*), INTENT( IN ) :: NEW_KEY
          CLASS(*), DIMENSION(:), INTENT( IN ) :: NEW_VALUE
 
          TYPE(G_LINK), POINTER :: NEW_LINK
          INTEGER :: Key_Len, Data_Type, N_Row, N_Col, Ini_Data

          PROCEDURE_NAME = "'PUSH_FRONT_V'"

          IF( ASSOCIATED( New_Link ) ) NULLIFY( New_link )


          SELECT TYPE( New_Value )
          TYPE IS ( INTEGER(1) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 1, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI1_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(2) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 2, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI2_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 3, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI4_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 4, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 11, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VR4_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 12, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VR8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 13, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VR16_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 21, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VC4_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 22, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VC8_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 23, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VC16_DATA( Ini_Data, New_Value )

          CLASS DEFAULT
       LOCAL_FLAG = "UNKNOWN DATA_TYPE SUPPLIED VALUE ="//INT_TO_STR(DATA_TYPE)
       WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )//" :: "
       WRITE(*,'(A)')BTRIM( LOCAL_FLAG )
          END SELECT

 
           IF (.NOT. ASSOCIATED(THIS%FIRST)) THEN
              THIS%FIRST => New_Link
              THIS%LAST => THIS%FIRST
           ELSE
             New_Link%Next => This%First
             THIS%FIRST => New_Link
           ENDIF
 
!******************************************************************************
         END SUBROUTINE PUSH_FRONT_V
!==============================================================================

!******************************************************************************
         SUBROUTINE PUSH_FRONT_M(THIS, NEW_KEY, NEW_VALUE)
!******************************************************************************
          IMPLICIT NONE
          CLASS(G_LIST) :: THIS
          CHARACTER(LEN=*), INTENT( IN ) :: NEW_KEY
          CLASS(*), DIMENSION(:,:), INTENT( IN ) :: NEW_VALUE
 
          TYPE(G_LINK), POINTER :: NEW_LINK
          INTEGER :: Key_Len, Data_Type, N_Row, N_Col, Ini_Data

          PROCEDURE_NAME = "'PUSH_FRONT_M'"

          IF( ASSOCIATED( New_Link ) ) NULLIFY( New_link )


          SELECT TYPE( New_Value )
          TYPE IS ( INTEGER(1) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 1, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI1_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(2) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 2, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI2_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 3, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI4_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 4, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 11, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MR4_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 12, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MR8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 13, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MR16_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 21, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MC4_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 22, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MC8_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 23, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MC16_DATA( Ini_Data, New_Value )

          CLASS DEFAULT
       LOCAL_FLAG = "UNKNOWN DATA_TYPE SUPPLIED VALUE ="//INT_TO_STR(DATA_TYPE)
       WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )//" :: "
       WRITE(*,'(A)')BTRIM( LOCAL_FLAG )
          END SELECT

 
           IF (.NOT. ASSOCIATED(THIS%FIRST)) THEN
              THIS%FIRST => New_Link
              THIS%LAST => THIS%FIRST
           ELSE
             New_Link%Next => This%First
             THIS%FIRST => New_Link
           ENDIF
 
!******************************************************************************
         END SUBROUTINE PUSH_FRONT_M
!==============================================================================

!******************************************************************************
         SUBROUTINE PUSH_Back_S(THIS, NEW_KEY, NEW_VALUE)
!******************************************************************************
          IMPLICIT NONE
          CLASS(G_LIST) :: THIS
          CHARACTER(LEN=*), INTENT( IN ) :: NEW_KEY
          CLASS(*), INTENT( IN ) :: NEW_VALUE
 
          TYPE(G_LINK), POINTER :: NEW_LINK
          INTEGER :: Key_Len, Data_Type, N_Row, N_Col, Ini_Data

          PROCEDURE_NAME = "'PUSH_Back_S'"

          IF( ASSOCIATED( New_Link ) ) NULLIFY( New_link )

          SELECT TYPE( New_Value )
          TYPE IS ( CHARACTER(LEN=*) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 0, LEN_BTRIM(NEW_VALUE), 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SS_DATA( Ini_Data, BTRIM( New_Value ) )

          TYPE IS ( INTEGER(1) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 1, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI1_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(2) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 2, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI2_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 3, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI4_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 4, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 11, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SR4_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY),12, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SR8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 13, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SR16_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 21, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SC4_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 22, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SC8_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 23, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SC16_DATA( Ini_Data, New_Value )

          CLASS DEFAULT
       LOCAL_FLAG = "UNKNOWN DATA_TYPE SUPPLIED VALUE ="//INT_TO_STR(DATA_TYPE)
       WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )//" :: "
       WRITE(*,'(A)')BTRIM( LOCAL_FLAG )
          END SELECT

 
           IF (.NOT. ASSOCIATED(THIS%FIRST)) THEN
              THIS%FIRST => New_Link
              THIS%LAST => THIS%FIRST
           ELSE
             THIS%LAST%NEXT => New_Link
             THIS%LAST => New_Link
           ENDIF
 
!******************************************************************************
         END SUBROUTINE PUSH_Back_S
!==============================================================================
 
!******************************************************************************
         SUBROUTINE PUSH_Back_V(THIS, NEW_KEY, NEW_VALUE)
!******************************************************************************
          IMPLICIT NONE
          CLASS(G_LIST) :: THIS
          CHARACTER(LEN=*), INTENT( IN ) :: NEW_KEY
          CLASS(*), DIMENSION(:), INTENT( IN ) :: NEW_VALUE
 
          TYPE(G_LINK), POINTER :: NEW_LINK
          INTEGER :: Key_Len, Data_Type, N_Row, N_Col, Ini_Data

          PROCEDURE_NAME = "'PUSH_Back_V'"

          IF( ASSOCIATED( New_Link ) ) NULLIFY( New_link )


          SELECT TYPE( New_Value )
          TYPE IS ( INTEGER(1) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 1, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI1_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(2) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 2, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI2_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 3, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI4_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 4, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 11, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VR4_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 12, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VR8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 13, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VR16_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 21, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VC4_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 22, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VC8_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 23, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VC16_DATA( Ini_Data, New_Value )

          CLASS DEFAULT
       LOCAL_FLAG = "UNKNOWN DATA_TYPE SUPPLIED VALUE ="//INT_TO_STR(DATA_TYPE)
       WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )//" :: "
       WRITE(*,'(A)')BTRIM( LOCAL_FLAG )
          END SELECT

 
           IF (.NOT. ASSOCIATED(THIS%FIRST)) THEN
              THIS%FIRST => New_Link
              THIS%LAST => THIS%FIRST
           ELSE
             THIS%LAST%NEXT => New_Link
             THIS%LAST => New_Link
           ENDIF
 
!******************************************************************************
         END SUBROUTINE PUSH_Back_V
!==============================================================================

!******************************************************************************
         SUBROUTINE PUSH_BACK_M(THIS, NEW_KEY, NEW_VALUE)
!******************************************************************************
          IMPLICIT NONE
          CLASS(G_LIST) :: THIS
          CHARACTER(LEN=*), INTENT( IN ) :: NEW_KEY
          CLASS(*), DIMENSION(:,:), INTENT( IN ) :: NEW_VALUE
 
          TYPE(G_LINK), POINTER :: NEW_LINK
          INTEGER :: Key_Len, Data_Type, N_Row, N_Col, Ini_Data

          PROCEDURE_NAME = "'PUSH_BACK_M'"

          IF( ASSOCIATED( New_Link ) ) NULLIFY( New_link )


          SELECT TYPE( New_Value )
          TYPE IS ( INTEGER(1) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 1, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI1_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(2) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 2, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI2_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 3, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI4_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 4, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 11, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MR4_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 12, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MR8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 13, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MR16_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 21, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MC4_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 22, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MC8_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 23, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MC16_DATA( Ini_Data, New_Value )

          CLASS DEFAULT
       LOCAL_FLAG = "UNKNOWN DATA_TYPE SUPPLIED VALUE ="//INT_TO_STR(DATA_TYPE)
       WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )//" :: "
       WRITE(*,'(A)')BTRIM( LOCAL_FLAG )
          END SELECT

 
           IF (.NOT. ASSOCIATED(THIS%FIRST)) THEN
              THIS%FIRST => New_Link
              THIS%LAST => THIS%FIRST
           ELSE
             THIS%LAST%NEXT => New_Link
             THIS%LAST => New_Link
           ENDIF
 
!******************************************************************************
         END SUBROUTINE PUSH_BACK_M
!==============================================================================
 
!******************************************************************************
         SUBROUTINE INSERT_FRONT_S(THIS, KEY, NEW_KEY, NEW_VALUE)
!******************************************************************************
          IMPLICIT NONE
           CLASS(G_LIST) :: THIS
         CHARACTER(LEN=*), INTENT( IN ) :: KEY
         CHARACTER(LEN=*), INTENT( IN ) :: NEW_KEY
          CLASS(*), INTENT( IN ) :: NEW_VALUE
 
         TYPE(G_LINK), POINTER :: CURR, PREV, NEW_LINK
         INTEGER(I1P) :: FOUND, MARK, CHOICE
         INTEGER :: Key_Len, Data_Type, N_Row, N_Col, Ini_Data

       PROCEDURE_NAME = "'INSERT_FRONT_S'"


          IF( ASSOCIATED( New_Link ) ) NULLIFY( New_link )

          SELECT TYPE( New_Value )
          TYPE IS ( CHARACTER(*) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 0, LEN_BTRIM(NEW_VALUE), 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SS_DATA( Ini_Data, BTRIM( New_Value ) )

          TYPE IS ( INTEGER(1) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 1, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI1_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(2) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 2, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI2_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 3, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI4_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 4, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 11, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SR4_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY),12, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SR8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 13, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SR16_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 21, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SC4_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 22, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SC8_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 23, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SC16_DATA( Ini_Data, New_Value )

          CLASS DEFAULT
       LOCAL_FLAG = "UNKNOWN DATA_TYPE SUPPLIED VALUE ="//INT_TO_STR(DATA_TYPE)
       WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )//" :: "
       WRITE(*,'(A)')BTRIM( LOCAL_FLAG )
          END SELECT

 
         FOUND = 0
 
             IF ( ASSOCIATED( THIS%FIRST ) ) THEN
 
         CURR => THIS%FIRST
         PREV => CURR
         MARK = 1
 
         DO WHILE(ASSOCIATED(CURR))
 
           IF( STRCMP( CURR%GIVE_KEY(), KEY ) )THEN
            FOUND = 1
             EXIT
           ELSE
           PREV => CURR
           CURR => CURR%NEXT
           MARK = 0
            ENDIF
         ENDDO
 
             ELSE
              THIS%FIRST => New_Link
              THIS%LAST => THIS%FIRST
       RETURN
             ENDIF
 
       IF( FOUND==1 .AND. MARK==0_I1P )THEN
       PREV%NEXT => NEW_LINK
       NEW_LINK%NEXT => CURR
 
       CURR => PREV
             DO WHILE(ASSOCIATED(CURR))
                   THIS%LAST => CURR
                   CURR => THIS%LAST%NEXT
             END DO
       ELSE
             New_Link%Next => This%First
             THIS%FIRST => New_Link
       ENDIF
 
!******************************************************************************
         END SUBROUTINE INSERT_FRONT_S
!==============================================================================
 
!******************************************************************************
         SUBROUTINE INSERT_FRONT_V(THIS, KEY, NEW_KEY, NEW_VALUE)
!******************************************************************************
          IMPLICIT NONE
           CLASS(G_LIST) :: THIS
         CHARACTER(LEN=*), INTENT( IN ) :: KEY
         CHARACTER(LEN=*), INTENT( IN ) :: NEW_KEY
          CLASS(*), DIMENSION(:), INTENT( IN ) :: NEW_VALUE
 
         TYPE(G_LINK), POINTER :: CURR, PREV, NEW_LINK
         INTEGER(I1P) :: FOUND, MARK, CHOICE
         INTEGER :: Key_Len, Data_Type, N_Row, N_Col, Ini_Data

       PROCEDURE_NAME = "'INSERT_FRONT_V'"


          IF( ASSOCIATED( New_Link ) ) NULLIFY( New_link )

          SELECT TYPE( New_Value )
          TYPE IS ( INTEGER(1) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 1, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI1_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(2) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 2, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI2_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 3, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI4_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 4, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 11, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VR4_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 12, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VR8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 13, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VR16_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 21, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VC4_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 22, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VC8_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 23, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VC16_DATA( Ini_Data, New_Value )

          CLASS DEFAULT
       LOCAL_FLAG = "UNKNOWN DATA_TYPE SUPPLIED VALUE ="//INT_TO_STR(DATA_TYPE)
       WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )//" :: "
       WRITE(*,'(A)')BTRIM( LOCAL_FLAG )
          END SELECT

 
         FOUND = 0
 
             IF ( ASSOCIATED( THIS%FIRST ) ) THEN
 
         CURR => THIS%FIRST
         PREV => CURR
         MARK = 1
 
         DO WHILE(ASSOCIATED(CURR))
 
           IF( STRCMP( CURR%Give_KEY(), KEY ) )THEN
            FOUND = 1
             EXIT
           ELSE
           PREV => CURR
           CURR => CURR%NEXT
           MARK = 0
            ENDIF
         ENDDO
 
             ELSE
              THIS%FIRST => New_Link
              THIS%LAST => THIS%FIRST
       RETURN
             ENDIF
 
       IF( FOUND==1 .AND. MARK==0_I1P )THEN
       PREV%NEXT => NEW_LINK
       NEW_LINK%NEXT => CURR
 
       CURR => PREV
             DO WHILE(ASSOCIATED(CURR))
                   THIS%LAST => CURR
                   CURR => THIS%LAST%NEXT
             END DO
       ELSE
             New_Link%Next => This%First
             THIS%FIRST => New_Link
       ENDIF
 
!******************************************************************************
         END SUBROUTINE INSERT_FRONT_V
!==============================================================================
 
!******************************************************************************
         SUBROUTINE INSERT_FRONT_M(THIS, KEY, NEW_KEY, NEW_VALUE)
!******************************************************************************
          IMPLICIT NONE
           CLASS(G_LIST) :: THIS
         CHARACTER(LEN=*), INTENT( IN ) :: KEY
         CHARACTER(LEN=*), INTENT( IN ) :: NEW_KEY
          CLASS(*), DIMENSION(:,:), INTENT( IN ) :: NEW_VALUE
 
         TYPE(G_LINK), POINTER :: CURR, PREV, NEW_LINK
         INTEGER(I1P) :: FOUND, MARK, CHOICE
         INTEGER :: Key_Len, Data_Type, N_Row, N_Col, Ini_Data

       PROCEDURE_NAME = "'INSERT_FRONT_M'"


          IF( ASSOCIATED( New_Link ) ) NULLIFY( New_link )

          SELECT TYPE( New_Value )
          TYPE IS ( INTEGER(1) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 1, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI1_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(2) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 2, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI2_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 3, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI4_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 4, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 11, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MR4_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 12, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MR8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 13, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MR16_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 21, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MC4_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 22, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MC8_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 23, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MC16_DATA( Ini_Data, New_Value )

          CLASS DEFAULT
       LOCAL_FLAG = "UNKNOWN DATA_TYPE SUPPLIED VALUE ="//INT_TO_STR(DATA_TYPE)
       WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )//" :: "
       WRITE(*,'(A)')BTRIM( LOCAL_FLAG )
          END SELECT

 
         FOUND = 0
 
             IF ( ASSOCIATED( THIS%FIRST ) ) THEN
 
         CURR => THIS%FIRST
         PREV => CURR
         MARK = 1
 
         DO WHILE(ASSOCIATED(CURR))
 
           IF( STRCMP( CURR%Give_KEY(), KEY ) )THEN
            FOUND = 1
             EXIT
           ELSE
           PREV => CURR
           CURR => CURR%NEXT
           MARK = 0
            ENDIF
         ENDDO
 
             ELSE
              THIS%FIRST => New_Link
              THIS%LAST => THIS%FIRST
       RETURN
             ENDIF
 
       IF( FOUND==1 .AND. MARK==0_I1P )THEN
       PREV%NEXT => NEW_LINK
       NEW_LINK%NEXT => CURR
 
       CURR => PREV
             DO WHILE(ASSOCIATED(CURR))
                   THIS%LAST => CURR
                   CURR => THIS%LAST%NEXT
             END DO
       ELSE
             New_Link%Next => This%First
             THIS%FIRST => New_Link
       ENDIF
 
!******************************************************************************
         END SUBROUTINE INSERT_FRONT_M
!==============================================================================
 
 
!******************************************************************************
         SUBROUTINE INSERT_BACK_S(THIS, KEY, NEW_KEY, NEW_VALUE)
!******************************************************************************
         IMPLICIT NONE
           CLASS(G_LIST) :: THIS
         CHARACTER(LEN=*), INTENT( IN ) :: KEY
         CHARACTER(LEN=*), INTENT( IN ) :: NEW_KEY
          CLASS(*), INTENT( IN ) :: NEW_VALUE
         INTEGER :: Key_Len, Data_Type, N_Row, N_Col, Ini_Data
 
         TYPE(G_LINK), POINTER :: CURR, NEXT, NEW_LINK
         INTEGER(I1P) :: FOUND, CHOICE

       PROCEDURE_NAME = "'INSERT_BACK_S'"
 
          IF( ASSOCIATED( New_Link ) ) NULLIFY( New_link )

          SELECT TYPE( New_Value )
          TYPE IS ( CHARACTER(*) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 0, LEN_BTRIM(NEW_VALUE), 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SS_DATA( Ini_Data, BTRIM( New_Value ) )

          TYPE IS ( INTEGER(1) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 1, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI1_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(2) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 2, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI2_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 3, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI4_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 4, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SI8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 11, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SR4_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY),12, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SR8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 13, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SR16_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 21, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SC4_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 22, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SC8_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 23, 1, 1)
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_SC16_DATA( Ini_Data, New_Value )

          CLASS DEFAULT
       LOCAL_FLAG = "UNKNOWN DATA_TYPE SUPPLIED VALUE ="//INT_TO_STR(DATA_TYPE)
       WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )//" :: "
       WRITE(*,'(A)')BTRIM( LOCAL_FLAG )
          END SELECT

         FOUND = 0
 
          IF ( ASSOCIATED( THIS%FIRST ) ) THEN
 
         CURR => THIS%FIRST
 
         DO WHILE(ASSOCIATED(CURR))
 
            IF( STRCMP( CURR%Give_KEY(), KEY ) )THEN
            FOUND = 1
             EXIT
            ELSE
           CURR => CURR%NEXT
            ENDIF
         ENDDO

          ELSE

              THIS%FIRST => New_Link
              THIS%LAST => THIS%FIRST
 
          ENDIF
 
       IF(FOUND/=0)THEN
 
       NEXT => CURR%NEXT
 
       CURR%NEXT => NEW_LINK
       NEW_LINK%NEXT => NEXT
 
             DO WHILE(ASSOCIATED(CURR))
                   THIS%LAST => CURR
                   CURR => THIS%LAST%NEXT
             END DO
       ELSE
 
             THIS%LAST%NEXT => New_Link
             THIS%LAST => New_Link
 
       ENDIF
 
!******************************************************************************
         END SUBROUTINE INSERT_BACK_S
!==============================================================================
 
!******************************************************************************
         SUBROUTINE INSERT_BACK_V(THIS, KEY, NEW_KEY, NEW_VALUE)
!******************************************************************************
         IMPLICIT NONE
           CLASS(G_LIST) :: THIS
         CHARACTER(LEN=*), INTENT( IN ) :: KEY
         CHARACTER(LEN=*), INTENT( IN ) :: NEW_KEY
          CLASS(*), DIMENSION(:), INTENT( IN ) :: NEW_VALUE
         INTEGER :: Key_Len, Data_Type, N_Row, N_Col, Ini_Data
 
         TYPE(G_LINK), POINTER :: CURR, NEXT, NEW_LINK
         INTEGER(I1P) :: FOUND, CHOICE

       PROCEDURE_NAME = "'INSERT_BACK_V'"
 
          IF( ASSOCIATED( New_Link ) ) NULLIFY( New_link )

          SELECT TYPE( New_Value )
          TYPE IS ( INTEGER(1) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 1, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI1_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(2) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 2, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI2_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 3, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI4_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 4, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VI8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 11, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VR4_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 12, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VR8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 13, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VR16_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 21, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VC4_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 22, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VC8_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 23, Size( New_Value, 1), 1 )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_VC16_DATA( Ini_Data, New_Value )

          CLASS DEFAULT
       LOCAL_FLAG = "UNKNOWN DATA_TYPE SUPPLIED VALUE ="//INT_TO_STR(DATA_TYPE)
       WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )//" :: "
       WRITE(*,'(A)')BTRIM( LOCAL_FLAG )
          END SELECT

         FOUND = 0
 
          IF ( ASSOCIATED( THIS%FIRST ) ) THEN
 
         CURR => THIS%FIRST
 
         DO WHILE(ASSOCIATED(CURR))
 
            IF( STRCMP( CURR%Give_KEY(), KEY ) )THEN
            FOUND = 1
             EXIT
            ELSE
           CURR => CURR%NEXT
            ENDIF
         ENDDO

          ELSE

              THIS%FIRST => New_Link
              THIS%LAST => THIS%FIRST
 
          ENDIF
 
       IF(FOUND/=0)THEN
 
       NEXT => CURR%NEXT
 
       CURR%NEXT => NEW_LINK
       NEW_LINK%NEXT => NEXT
 
             DO WHILE(ASSOCIATED(CURR))
                   THIS%LAST => CURR
                   CURR => THIS%LAST%NEXT
             END DO
       ELSE
 
             THIS%LAST%NEXT => New_Link
             THIS%LAST => New_Link
 
       ENDIF
 
!******************************************************************************
         END SUBROUTINE INSERT_BACK_V
!==============================================================================
 
!******************************************************************************
         SUBROUTINE INSERT_BACK_M(THIS, KEY, NEW_KEY, NEW_VALUE)
!******************************************************************************
         IMPLICIT NONE
           CLASS(G_LIST) :: THIS
         CHARACTER(LEN=*), INTENT( IN ) :: KEY
         CHARACTER(LEN=*), INTENT( IN ) :: NEW_KEY
          CLASS(*), DIMENSION(:,:), INTENT( IN ) :: NEW_VALUE
         INTEGER :: Key_Len, Data_Type, N_Row, N_Col, Ini_Data
 
         TYPE(G_LINK), POINTER :: CURR, NEXT, NEW_LINK
         INTEGER(I1P) :: FOUND, CHOICE

       PROCEDURE_NAME = "'INSERT_BACK_M'"
 
          IF( ASSOCIATED( New_Link ) ) NULLIFY( New_link )

          SELECT TYPE( New_Value )

          TYPE IS ( INTEGER(1) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 1, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI1_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(2) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 2, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI2_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 3, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI4_DATA( Ini_Data, New_Value )

          TYPE IS ( INTEGER(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 4, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MI8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 11, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MR4_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 12, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MR8_DATA( Ini_Data, New_Value )

          TYPE IS ( REAL(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 13, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MR16_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(4) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 21, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MC4_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(8) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 22, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MC8_DATA( Ini_Data, New_Value )

          TYPE IS ( COMPLEX(16) )
          New_Link => G_LINK( LEN_BTRIM(NEW_KEY), 23, Size( New_Value, 1), SIZE( New_Value, 2 ) )
          CALL New_Link%Give_CONTENT(  Key_Len, Data_Type, N_Row, N_Col, Ini_Data ) 
          CALL New_Link%SET_KEY( BTRIM( New_Key ) )
          CALL New_Link%INSERT_MC16_DATA( Ini_Data, New_Value )

          CLASS DEFAULT
       LOCAL_FLAG = "UNKNOWN DATA_TYPE SUPPLIED VALUE ="//INT_TO_STR(DATA_TYPE)
       WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )//" :: "
       WRITE(*,'(A)')BTRIM( LOCAL_FLAG )
          END SELECT

         FOUND = 0
 
          IF ( ASSOCIATED( THIS%FIRST ) ) THEN
 
         CURR => THIS%FIRST
 
         DO WHILE(ASSOCIATED(CURR))
 
            IF( STRCMP( CURR%Give_KEY(), KEY ) )THEN
            FOUND = 1
             EXIT
            ELSE
           CURR => CURR%NEXT
            ENDIF
         ENDDO

          ELSE

              THIS%FIRST => New_Link
              THIS%LAST => THIS%FIRST
 
          ENDIF
 
       IF(FOUND/=0)THEN
 
       NEXT => CURR%NEXT
 
       CURR%NEXT => NEW_LINK
       NEW_LINK%NEXT => NEXT
 
             DO WHILE(ASSOCIATED(CURR))
                   THIS%LAST => CURR
                   CURR => THIS%LAST%NEXT
             END DO
       ELSE
 
             THIS%LAST%NEXT => New_Link
             THIS%LAST => New_Link
 
       ENDIF
 
!******************************************************************************
         END SUBROUTINE INSERT_BACK_M
!==============================================================================
 
!******************************************************************************
         FUNCTION IS_EMPTY(THIS)
!******************************************************************************
          IMPLICIT NONE
           CLASS(G_LIST) :: THIS
           LOGICAL(LGT) :: IS_EMPTY
 
           IF (ASSOCIATED(THIS%FIRST)) THEN
              IS_EMPTY = .FALSE.
           ELSE
              IS_EMPTY = .TRUE.
           ENDIF
!******************************************************************************
       END FUNCTION IS_EMPTY
!==============================================================================
 
!******************************************************************************
         FUNCTION HAS_KEY(THIS, KEY) RESULT(IS_IN_LIST)
!******************************************************************************
         IMPLICIT NONE
           CLASS(G_LIST) :: THIS
         CHARACTER(LEN=*), INTENT( IN ) :: KEY
         LOGICAL(LGT) :: IS_IN_LIST
         TYPE(G_LINK), POINTER :: CURR
 
         IS_IN_LIST = .FALSE.
           IF (.NOT.THIS%IS_EMPTY()) THEN
 
         CURR => THIS%FIRST
         DO WHILE(ASSOCIATED(CURR))
 
           IF( STRCMP( CURR%GIVE_KEY(), KEY ) )THEN
             IS_IN_LIST = .TRUE.
            RETURN
            ELSE
           CURR => CURR%NEXT
            ENDIF
         ENDDO
           ENDIF
!******************************************************************************
         END FUNCTION HAS_KEY
!==============================================================================
 
!******************************************************************************
         FUNCTION GIVE_LINK_AT(THIS, KEY) 
!******************************************************************************
         IMPLICIT NONE
           CLASS(G_LIST) :: THIS
         CHARACTER(LEN=*), INTENT( IN ) :: KEY
         TYPE(G_LINK), POINTER :: GIVE_LINK_AT

        PROCEDURE_NAME = "'GIVE_LINK_AT'"
 
         IF( ASSOCIATED( GIVE_LINK_AT ) ) NULLIFY( GIVE_LINK_AT )
 
           IF (.NOT.THIS%IS_EMPTY()) THEN
 
         GIVE_LINK_AT => THIS%FIRST
         DO WHILE(ASSOCIATED(GIVE_LINK_AT))
 
           IF( STRCMP( GIVE_LINK_AT%GIVE_KEY(), KEY ) )THEN
            RETURN
            ELSE
           GIVE_LINK_AT => GIVE_LINK_AT%NEXT
            ENDIF
         ENDDO
           ENDIF

!******************************************************************************
         END FUNCTION GIVE_LINK_AT
!==============================================================================
 
!******************************************************************************
         SUBROUTINE ERASE_LINK_AT(THIS, KEY)
!******************************************************************************
          IMPLICIT NONE
           CLASS(G_LIST) :: THIS
         CHARACTER(LEN=*), INTENT( IN ) :: KEY
         TYPE(G_LINK), POINTER :: CURR, PREV, TEMP
         INTEGER(I1P) :: FOUND, MARK

        PROCEDURE_NAME = "'ERASE_LINK_AT'"
 
         FOUND = 0
 
             IF (.NOT.THIS%IS_EMPTY()) THEN
 
         CURR => THIS%FIRST
         PREV => CURR
         MARK = 1
 
         DO WHILE(ASSOCIATED(CURR))
 
           IF( STRCMP( CURR%GIVE_KEY(), KEY ) )THEN
            FOUND = 1
             EXIT
           ELSE
           PREV => CURR
           CURR => CURR%NEXT
           MARK = 0
            ENDIF
         ENDDO
 
      IF(FOUND==0)RETURN
 
       TEMP => CURR
       CURR => CURR%NEXT
 
!---------------------[ DELETE THE LINK FROM MEMORY ]---------------------------
       CALL TEMP%ERASE()
      IF( ASSOCIATED( TEMP ) ) DEALLOCATE( TEMP )
!-------------------------------------------------------------------------------
 
       IF((FOUND/=0).AND.(MARK==0_I1P))THEN
       PREV%NEXT => CURR
 
             DO WHILE(ASSOCIATED(PREV))
                   THIS%LAST => PREV
                   PREV => THIS%LAST%NEXT
             END DO
       ELSEIF((FOUND/=0).AND.(MARK==1_I1P))THEN
       THIS%FIRST => CURR
       ENDIF
 
             ENDIF
 
!******************************************************************************
       END SUBROUTINE ERASE_LINK_AT
!==============================================================================
 
!******************************************************************************
       SUBROUTINE ERASE(THIS)
!******************************************************************************
       IMPLICIT NONE
           CLASS(G_LIST) :: THIS
           TYPE(G_LINK), POINTER :: TEMP
 
           IF( .NOT.THIS%IS_EMPTY() )THEN
             THIS%LAST => NULL()
           DO WHILE (ASSOCIATED(THIS%FIRST))
             TEMP => THIS%FIRST
             THIS%FIRST => THIS%FIRST%NEXT
!---------------------[ DELETE THE LINK FROM MEMORY ]---------------------------
       CALL TEMP%ERASE()
      IF( ASSOCIATED( TEMP ) ) DEALLOCATE( TEMP )
!-------------------------------------------------------------------------------
             END DO
 
             THIS%FIRST => NULL()
           ENDIF
 
!******************************************************************************
       END SUBROUTINE ERASE
!==============================================================================

!******************************************************************************
       SUBROUTINE ERROR_CODE(IER, FLAG )
!******************************************************************************
       IMPLICIT NONE
       INTEGER(I1P), OPTIONAL, INTENT(IN) :: IER
       CHARACTER(LEN=*), OPTIONAL, INTENT( IN ) :: FLAG

       IF( PRESENT( FLAG ) )THEN
       WRITE(*,'(A)' ) FLAG
       END IF

       WRITE(*,'(A)',ADVANCE='NO')" :: "//CLASS_NAME//" :: "//BTRIM( PROCEDURE_NAME )

           IF( PRESENT(IER) )THEN
       IF( IER==0 )THEN
       WRITE(*,'(A)' ) BTRIM( LOCAL_FLAG )
       WRITE(*,'(A)' )  "*** EXECUTION SUCCESSFUL !! ***"
       ELSEIF( IER==1 )THEN
       WRITE(*,'(A)' ) BTRIM( LOCAL_FLAG )
       WRITE(*,'(A)' )  "******  UNABLE TO FIND KEY/INDEX !! ******"
       ELSEIF( IER==2 )THEN
       WRITE(*,'(A)' ) BTRIM( LOCAL_FLAG )
       WRITE(*,'(A)' )  "******  LINK VALUE DOESNOT EXIST !! ******"
       ELSEIF( IER==3 )THEN
       WRITE(*,'(A)' ) BTRIM( LOCAL_FLAG )
       WRITE(*,'(A)' )  "******  LIST IS EMPTY !! ******"
       ELSEIF( IER==4 )THEN
       WRITE(*,'(A)' ) BTRIM( LOCAL_FLAG )
       WRITE(*,'(A)' )  "****** 'RESET_KEY' FAILED  !! ******"
       ELSE
       WRITE(*,'(A)' ) BTRIM( LOCAL_FLAG )
       WRITE(*,'(A)' )  "******  UNKNOWN INTERNAL ERROR !! ******"
       END IF
           ELSE
       WRITE(*,'(A)' ) BTRIM( LOCAL_FLAG )
       WRITE(*,'(A)' )  "******  LAST EXECUTED PROCEEDURE ******"
           ENDIF

!******************************************************************************
       END SUBROUTINE ERROR_CODE
!==============================================================================
 
!----------------------------[ LIST FUNCTIONS ARE ABOVE ]---------------------------------
 
!******************************************************************************
       END MODULE CLASS_G_LIST
!==============================================================================
 
 
 
