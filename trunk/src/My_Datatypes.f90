       module My_DataTypes
       implicit none

! --------------  Define available precisions below -------------------

!---------   r e a l  d a t a t y p e s ---------------
       INTEGER(2), PARAMETER:: R16P = SELECTED_REAL_KIND(33,4931)
       INTEGER(2), PARAMETER:: R8P  = SELECTED_REAL_KIND(15,307)  
       INTEGER(2), PARAMETER:: R4P  = SELECTED_REAL_KIND(6,37)    

!---------   i n t e g e r  d a t a t y p e s ---------------
       INTEGER(2), PARAMETER:: I8P  = SELECTED_INT_KIND(18)
       INTEGER(2), PARAMETER:: I4P  = SELECTED_INT_KIND(9)  
       INTEGER(2), PARAMETER:: I2P  = SELECTED_INT_KIND(4)  
       INTEGER(2), PARAMETER:: I1P  = SELECTED_INT_KIND(2)  
!======================================================================

       INTEGER, PARAMETER :: DP = R8P
       INTEGER, PARAMETER::LGT=KIND(.TRUE.)

       INTEGER, PARAMETER :: PtrLoc_t = I8P
       INTEGER, PARAMETER :: IO_t = I2P
       INTEGER, PARAMETER :: ERROR_t = I1P

       REAL(R8P),  PARAMETER:: SMALL_R8P  = TINY(1.0_R8P ) 
       REAL(R4P),  PARAMETER:: SMALL_R4P  = TINY(1.0_R4P )
       
       REAL(R8P),  PARAMETER:: ZERO_R8P  = NEAREST(1.0_R8P, 1.0_R8P) - &
                                           NEAREST(1.0_R8P,-1.0_R8P)
       REAL(R4P),  PARAMETER:: ZERO_R4P  = NEAREST(1.0_R4P, 1.0_R4P) - &
                                           NEAREST(10._R4P,-1.0_R4P)

       REAL(R8P),  PARAMETER:: ONE_R8P  =  1.0_R8P
       REAL(R4P),  PARAMETER:: ONE_R4P  =  1.0_R4P 
 
!*******************************************************************************
 
         REAL(R8P),PARAMETER::                                    &
                       PI_R8P     = 3.1415926535897932_R8P        &
                    , TWOPI_R8P   = 6.2831853071795865_R8P        &
                    , DEG2RAD_R8P = 0.0174532925199433_R8P        &
                    , RAD2DEG_R8P = 57.2957795130823209_R8P
         REAL(R4P),PARAMETER::                                    &
                       PI_R4P     = 3.1415927_R4P                 &
                    , TWOPI_R4P   = 6.2831853_R4P                 &
                    , DEG2RAD_R4P = 0.0174533_R4P                 &
                    , RAD2DEG_R4P = 57.2957795_R4P
 
 
         REAL(DP),PARAMETER::                     &
                       PI     = PI_R8P            &
                    , TWOPI   = TWOPI_R8P         &
                    , DEG2RAD = DEG2RAD_R8P       &
                    , RAD2DEG = RAD2DEG_R8P       &
                    , SMALL_R = SMALL_R8P         &
                    , ZERO    = ZERO_R8P          &
                    , ONE     = ONE_R8P
 
         INTEGER, PARAMETER :: MAX_PATH_LEN=512
         INTEGER, PARAMETER :: MAX_STRING_LEN=256
         INTEGER, PARAMETER :: MAX_NAME_LEN = 128
         INTEGER, PARAMETER :: MAX_PROCEDURE_LEN = 56
         INTEGER, PARAMETER :: MAX_KEY_LEN = 32
         INTEGER, PARAMETER :: MAX_ID_LEN = 100000
         INTEGER, PARAMETER :: MAX_BC_LEN = 1000
         INTEGER, PARAMETER :: Int_Byte = KIND(0)
         INTEGER,PARAMETER::IFILE_SCRATCH=10001
         CHARACTER(1), PARAMETER :: Backslash = ACHAR(92)
         CHARACTER(1), PARAMETER :: NullChar = CHAR(0)
 
 
         TYPE COMPLEX_DATA
          REAL(DP)::RE,IM
         END TYPE COMPLEX_DATA
 
         INTEGER, PARAMETER :: kibibyte = 1024
         INTEGER, PARAMETER :: mebibyte = 1048576
         INTEGER, PARAMETER :: gibibyte = 1073741824

       end module My_DataTypes
