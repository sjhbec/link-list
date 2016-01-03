!***********************************************************************
      MODULE String_Tools
!***********************************************************************
      USE My_Datatypes, ONLY : I2P
      IMPLICIT NONE
      PRIVATE
      PUBLIC:: LEN_BTRIM, BTRIM, STRADD, STRCMP, TOUPPER, ToLower, &
               INT_TO_STR

      INTERFACE INT_TO_STR
      MODULE PROCEDURE               &
                       I8_TO_STR,    &
                       I4_TO_STR,    &
                       I2_TO_STR,    &
                       I1_TO_STR
      END INTERFACE INT_TO_STR
 
      INTERFACE STRCMP
      MODULE PROCEDURE STRCMP_10, STRCMP_20, STRCMP_30, STRCMP_40
      END INTERFACE STRCMP
 
 
!-----------------------------------------------------------------------
      CONTAINS
!-----------------------------------------------------------------------
 
!***********************************************************************
      PURE FUNCTION LEN_BTRIM(A) RESULT(N)
!***********************************************************************
      IMPLICIT NONE
        CHARACTER(LEN=*),INTENT(IN)::A
        INTEGER :: N
      N=LEN_TRIM(ADJUSTL(A))
!=======================================================================
      END FUNCTION LEN_BTRIM
!=======================================================================
 
!***********************************************************************
      PURE FUNCTION BTRIM(A) RESULT(STR)
!***********************************************************************
      IMPLICIT NONE
        CHARACTER(LEN=*),INTENT(IN)::A
        CHARACTER(LEN_BTRIM(A))::STR
      STR=TRIM(ADJUSTL(A))
!=======================================================================
      END FUNCTION BTRIM
!=======================================================================
 
!***********************************************************************
      SUBROUTINE STRADD(A,B)
!***********************************************************************
      IMPLICIT NONE
        CHARACTER(LEN=*),INTENT(IN)::B
        CHARACTER(LEN=*),INTENT(INOUT)::A
      A = BTRIM(A)//BTRIM(B)
!=======================================================================
      END SUBROUTINE STRADD
!=======================================================================
 
!******************************************************************************
        PURE FUNCTION TOUPPER(STRING_O) RESULT(STRING_N)
!******************************************************************************
        IMPLICIT NONE
        INTEGER, PARAMETER :: A=ICHAR('a'), Z=ICHAR('z'), L2U=ICHAR('A')-ICHAR('a')
        CHARACTER(LEN=*),INTENT(IN)::STRING_O
        CHARACTER(LEN(STRING_O))::STRING_N
        INTEGER :: I, J, N
        CHARACTER(1) :: C
 
        N = LEN(STRING_O)
 
                 DO I=1,N
          J = ICHAR(STRING_O(I:I))
        IF( ( J >= a ) .AND. ( J <= z )  )THEN
       C = ACHAR(J + L2U )
        ELSE
       C = ACHAR(J)
        ENDIF
       STRING_N(I:I) = C
 
                 END DO
 
!******************************************************************************
        END FUNCTION TOUPPER
!==============================================================================
 
!******************************************************************************
        PURE FUNCTION ToLower(STRING_O) RESULT(STRING_N)
!******************************************************************************
        IMPLICIT NONE
        INTEGER, PARAMETER :: A=ICHAR('A'), Z=ICHAR('Z'), U2L=ICHAR('a')-ICHAR('A')
        CHARACTER(LEN=*),INTENT(IN)::STRING_O
        CHARACTER(LEN(STRING_O))::STRING_N
        INTEGER :: I, J, N
        CHARACTER(1) :: C
 
        N = LEN(STRING_O)
 
                 DO I=1,N
          J = ICHAR(STRING_O(I:I))
        IF( ( J >= A ) .AND. ( J <= Z )  )THEN
       C = ACHAR(J + U2L )
        ELSE
       C = ACHAR(J)
        ENDIF
       STRING_N(I:I) = C
 
                 END DO
 
!******************************************************************************
       END FUNCTION ToLower
!==============================================================================
 
!******************************************************************************
        PURE FUNCTION STRCMP_10(STRING_O,STRING_N) RESULT(VAL)
!******************************************************************************
        IMPLICIT NONE
        INTEGER( I2P ), PARAMETER :: A=ICHAR('a'), Z=ICHAR('z'), L2U=ICHAR('A')-ICHAR('a')
        CHARACTER(LEN=*),INTENT(IN)::STRING_O
        CHARACTER(LEN=*),INTENT(IN)::STRING_N
        LOGICAL(KIND(.TRUE.)) :: VAL

        INTEGER( I2P ) :: I
        INTEGER( I2P ) :: I_1, INI_1, N_1
        INTEGER( I2P ) :: I_2, INI_2, N_2

        VAL = .FALSE.

!----------------[ TRYING FOR POSSIBLE TRIMING FROM BOTH SIDES ]----------------

        N_1 = LEN_TRIM( STRING_O )
        INI_1 = N_1 - LEN_TRIM( ADJUSTL( STRING_O ) )
        N_1 = N_1 - INI_1
        IF( N_1 <= 0_I2P ) RETURN       

!--------------------------------------------------------------------------------
!----------------[ TRYING FOR POSSIBLE TRIMING FROM BOTH SIDES ]----------------

        N_2 = LEN_TRIM( STRING_N )
        INI_2 = N_2 - LEN_TRIM( ADJUSTL( STRING_N ) )
        N_2 = N_2 - INI_2
        IF( N_2 <= 0_I2P ) RETURN
        
!--------------------------------------------------------------------------------
        IF( N_1/=N_2 ) RETURN
 
        VAL = .TRUE.

        LOOP_CHECK: DO I = 1_I2P, N_1

          I_1 = ICHAR( STRING_O( INI_1 + I: INI_1 + I ) )
        IF( ( I_1 >= A ) .AND. ( I_1 <= Z )  ) I_1 = I_1 + L2U
        
          I_2 = ICHAR( STRING_N( INI_2 + I: INI_2 + I ) )
        IF( ( I_2 >= A ) .AND. ( I_2 <= Z )  ) I_2 = I_2 + L2U

           IF( I_1/=I_2 )THEN
        VAL = .FALSE.
        RETURN
           ENDIF
        END DO LOOP_CHECK
!******************************************************************************
        END FUNCTION STRCMP_10
!==============================================================================
 
!******************************************************************************
        PURE FUNCTION STRCMP_20(STRING_O,STRING_N) RESULT(VAL)
!******************************************************************************
        IMPLICIT NONE
        INTEGER( I2P ), PARAMETER :: A=ICHAR('a'), Z=ICHAR('z'), L2U=ICHAR('A')-ICHAR('a')
        CHARACTER(LEN=*),INTENT(IN)::STRING_O
        CHARACTER(1),DIMENSION(:),INTENT(IN)::STRING_N
        LOGICAL(KIND(.TRUE.)) :: VAL

        INTEGER( I2P ) :: I
        INTEGER( I2P ) :: I_1, INI_1, N_1
        INTEGER( I2P ) :: I_2, INI_2, FNL_2, N_2

        VAL = .FALSE.

!----------------[ TRYING FOR POSSIBLE TRIMING FROM BOTH SIDES ]----------------
       
        N_1 = LEN_TRIM( STRING_O )
        INI_1 = N_1 - LEN_TRIM( ADJUSTL( STRING_O ) )
        N_1 = N_1 - INI_1
        IF( N_1 <= 0_I2P ) RETURN
        
!--------------------------------------------------------------------------------
!----------------[ TRYING FOR POSSIBLE TRIMING FROM BOTH SIDES ]----------------
        N_2 = SIZE( STRING_N, 1 )
        
        INI_2 = 0_I2P
        DO I = 1_I2P, N_2
        IF( STRING_N( INI_2 + 1_I2P )/= ' ' )EXIT
        INI_2 = INI_2 + 1_I2P
        END DO
        
        FNL_2 = N_2 + 1_I2P
        DO I = 1_I2P, N_2
        FNL_2 = FNL_2 - 1_I2P
        IF( STRING_N(FNL_2)/= ' ' )EXIT
        END DO
        
        N_2 = FNL_2 - INI_2

        IF( N_2 <= 0_I2P ) RETURN
        
!--------------------------------------------------------------------------------
 
        IF( N_1/=N_2 ) RETURN
 
        VAL = .TRUE.

        LOOP_CHECK: DO I = 1_I2P, N_1

          I_1 = ICHAR( STRING_O( INI_1 + I: INI_1 + I ) )
        IF( ( I_1 >= A ) .AND. ( I_1 <= Z )  ) I_1 = I_1 + L2U
        
          I_2 = ICHAR( STRING_N( INI_2 + I ) )
        IF( ( I_2 >= A ) .AND. ( I_2 <= Z )  ) I_2 = I_2 + L2U

           IF( I_1/=I_2 )THEN
        VAL = .FALSE.
        RETURN
           ENDIF
        END DO LOOP_CHECK
!******************************************************************************
        END FUNCTION STRCMP_20
!==============================================================================
 
!******************************************************************************
        PURE FUNCTION STRCMP_30(STRING_O,STRING_N) RESULT(VAL)
!******************************************************************************
        IMPLICIT NONE
        INTEGER( I2P ), PARAMETER :: A=ICHAR('a'), Z=ICHAR('z'), L2U=ICHAR('A')-ICHAR('a')
        CHARACTER(1),DIMENSION(:),INTENT(IN)::STRING_O
        CHARACTER(LEN=*),INTENT(IN)::STRING_N
        LOGICAL(KIND(.TRUE.)) :: VAL

        INTEGER( I2P ) :: I
        INTEGER( I2P ) :: I_1, INI_1, FNL_1, N_1
        INTEGER( I2P ) :: I_2, INI_2, N_2

        VAL = .FALSE.

!----------------[ TRYING FOR POSSIBLE TRIMING FROM BOTH SIDES ]----------------
        N_1 = SIZE( STRING_O, 1 )
        
        INI_1 = 0_I2P
        DO I = 1_I2P, N_1
        IF( STRING_O( INI_1 + 1_I2P )/= ' ' )EXIT
        INI_1 = INI_1 + 1_I2P
        END DO
        
        FNL_1 = N_1 + 1_I2P
        DO I = 1_I2P, N_1
        FNL_1 = FNL_1 - 1_I2P
        IF( STRING_O(FNL_1)/= ' ' )EXIT
        END DO
        
        N_1 = FNL_1 - INI_1

        IF( N_1 <= 0_I2P ) RETURN
        
!--------------------------------------------------------------------------------
!----------------[ TRYING FOR POSSIBLE TRIMING FROM BOTH SIDES ]----------------
       
        N_2 = LEN_TRIM( STRING_N )
        INI_2 = N_2 - LEN_TRIM( ADJUSTL( STRING_N ) )
        N_2 = N_2 - INI_2
        IF( N_2 <= 0_I2P ) RETURN
        
!--------------------------------------------------------------------------------
 
        IF( N_1/=N_2 ) RETURN
 
        VAL = .TRUE.

        LOOP_CHECK: DO I = 1_I2P, N_1

          I_1 = ICHAR( STRING_O( INI_1 + I ) )
        IF( ( I_1 >= A ) .AND. ( I_1 <= Z )  ) I_1 = I_1 + L2U
        
          I_2 = ICHAR( STRING_N( INI_2 + I: INI_2 + I ) )
        IF( ( I_2 >= A ) .AND. ( I_2 <= Z )  ) I_2 = I_2 + L2U

           IF( I_1/=I_2 )THEN
        VAL = .FALSE.
        RETURN
           ENDIF
        END DO LOOP_CHECK
!******************************************************************************
        END FUNCTION STRCMP_30
!==============================================================================
 
!******************************************************************************
        PURE FUNCTION STRCMP_40(STRING_O,STRING_N) RESULT(VAL)
!******************************************************************************
        IMPLICIT NONE
        INTEGER( I2P ), PARAMETER :: A=ICHAR('a'), Z=ICHAR('z'), L2U=ICHAR('A')-ICHAR('a')
        CHARACTER(1),DIMENSION(:),INTENT(IN) :: STRING_O
        CHARACTER(1),DIMENSION(:),INTENT(IN) :: STRING_N
        LOGICAL(KIND(.TRUE.)) :: VAL

        INTEGER( I2P ) :: I
        INTEGER( I2P ) :: I_1, INI_1, FNL_1, N_1
        INTEGER( I2P ) :: I_2, INI_2, FNL_2, N_2

        VAL = .FALSE.

!----------------[ TRYING FOR POSSIBLE TRIMING FROM BOTH SIDES ]----------------
        N_1 = SIZE( STRING_O, 1 )
        
        INI_1 = 0_I2P
        DO I = 1_I2P, N_1
        IF( STRING_O( INI_1 + 1_I2P )/= ' ' )EXIT
        INI_1 = INI_1 + 1_I2P
        END DO
        
        FNL_1 = N_1 + 1_I2P
        DO I = 1_I2P, N_1
        FNL_1 = FNL_1 - 1_I2P
        IF( STRING_O(FNL_1)/= ' ' )EXIT
        END DO
        
        N_1 = FNL_1 - INI_1

        IF( N_1 <= 0_I2P ) RETURN
        
!--------------------------------------------------------------------------------
!----------------[ TRYING FOR POSSIBLE TRIMING FROM BOTH SIDES ]----------------
        N_2 = SIZE( STRING_N, 1 )
        
        INI_2 = 0_I2P
        DO I = 1_I2P, N_2
        IF( STRING_N( INI_2 + 1_I2P )/= ' ' )EXIT
        INI_2 = INI_2 + 1_I2P
        END DO
        
        FNL_2 = N_2 + 1_I2P
        DO I = 1_I2P, N_2
        FNL_2 = FNL_2 - 1_I2P
        IF( STRING_N(FNL_2)/= ' ' )EXIT
        END DO
        
        N_2 = FNL_2 - INI_2

        IF( N_2 <= 0_I2P ) RETURN
        
!--------------------------------------------------------------------------------
 
        IF( N_1/=N_2 ) RETURN
 
        VAL = .TRUE.

        LOOP_CHECK: DO I = 1_I2P, N_1

          I_1 = ICHAR( STRING_O( INI_1 + I ) )
        IF( ( I_1 >= A ) .AND. ( I_1 <= Z )  ) I_1 = I_1 + L2U
        
          I_2 = ICHAR( STRING_N( INI_2 + I ) )
        IF( ( I_2 >= A ) .AND. ( I_2 <= Z )  ) I_2 = I_2 + L2U

           IF( I_1/=I_2 )THEN
        VAL = .FALSE.
        RETURN
           ENDIF
        END DO LOOP_CHECK
!******************************************************************************
        END FUNCTION STRCMP_40
!==============================================================================
 
!***********************************************************************
        ELEMENTAL FUNCTION I1_TO_STR(N) RESULT(STR)
!***********************************************************************
        IMPLICIT NONE
        INTEGER(1), INTENT(IN) ::     N     !  INTEGER TO BE CONVERTED.
        CHARACTER(4)           ::   STR     !  4 CHARACTERS ARE NEEDED FOR INTEGER(1); 1 FOR SIGN + 3 DIGITS
! INTEGER(1) values range from -128 to 127 and are stored in 1 contiguous bytes:  2^(1X8=8) => [ -2^(7), 2^(7)-1], 3  DIGITS PLUS SIGN; 8  BITS.
!---------------------------------------------------------------------------------------------------------------------------------
 
!---------------------------------------------------------------------------------------------------------------------------------
        WRITE(STR,'(I4)') N
            IF (N>=0)THEN
              STR = '+'//BTRIM( STR(2:) )
            ELSE
              STR = '-'//BTRIM( STR(2:) )
            ENDIF
!=======================================================================
        END FUNCTION I1_TO_STR
!=======================================================================
 
!***********************************************************************
        ELEMENTAL FUNCTION I2_TO_STR(N) RESULT(STR)
!***********************************************************************
        IMPLICIT NONE
        INTEGER(2), INTENT(IN) ::     N     ! INTEGER TO BE CONVERTED.
        CHARACTER(6)           ::   STR     !  6 CHARACTERS ARE NEEDED FOR INTEGER(2); 1 FOR SIGN + 5 DIGITS
! INTEGER(2) values range from -32,768 to 32,767 and are stored in 2 contiguous bytes:  2^(2X8=16) => [ -2^(15), 2^(15)-1], 5  DIGITS PLUS SIGN; 16 BITS.
!---------------------------------------------------------------------------------------------------------------------------------
 
!---------------------------------------------------------------------------------------------------------------------------------
        WRITE(STR,'(I6)') N
            IF (N>=0)THEN
              STR = '+'//BTRIM( STR(2:) )
            ELSE
              STR = '-'//BTRIM( STR(2:) )
            ENDIF
!=======================================================================
        END FUNCTION I2_TO_STR
!=======================================================================
 
!***********************************************************************
        ELEMENTAL FUNCTION I4_TO_STR(N) RESULT(STR)
!***********************************************************************
        IMPLICIT NONE
        INTEGER(4), INTENT(IN) ::     N     ! INTEGER TO BE CONVERTED.
        CHARACTER(11)          ::   STR     ! 11 CHARACTERS ARE NEEDED FOR INTEGER(4); 1 FOR SIGN + 10 DIGITS
! INTEGER(4) values range from -2,147,483,648 to 2,147,483,647 and are stored in 4 contiguous bytes 2^(4X8=32) => [ -2^(31), 2^(31)-1], 10 DIGITS PLUS SIGN; 32 BITS.
!---------------------------------------------------------------------------------------------------------------------------------
 
!---------------------------------------------------------------------------------------------------------------------------------
        WRITE(STR,'(I11)') N
            IF (N>=0)THEN
              STR = '+'//BTRIM( STR(2:) )
            ELSE
              STR = '-'//BTRIM( STR(2:) )
            ENDIF
!=======================================================================
        END FUNCTION I4_TO_STR
!=======================================================================
 
!***********************************************************************
        ELEMENTAL FUNCTION I8_TO_STR(N) RESULT(STR)
!***********************************************************************
        IMPLICIT NONE
        INTEGER(8), INTENT(IN) ::     N     ! INTEGER TO BE CONVERTED.
        CHARACTER(20)          ::   STR     ! 20 CHARACTERS ARE NEEDED FOR INTEGER(8); 1 FOR SIGN + 19 DIGITS
! INTEGER(8) values range from -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807 and are stored in 8 contiguous bytes 2^(8X8=64) => [ -2^(63), 2^(63)-1], 19 DIGITS PLUS SIGN; 64 BITS.
!---------------------------------------------------------------------------------------------------------------------------------
 
!---------------------------------------------------------------------------------------------------------------------------------
        WRITE(STR,'(I20)') N
            IF (N>=0)THEN
              STR = '+'//BTRIM( STR(2:) )
            ELSE
              STR = '-'//BTRIM( STR(2:) )
            ENDIF
!=======================================================================
        END FUNCTION I8_TO_STR
!=======================================================================
 
!=======================================================================
      END MODULE String_Tools
!=======================================================================
