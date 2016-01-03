!************************************************************************          
          Module Type_Conversion
!************************************************************************
          USE My_Datatypes, ONLY : I1P, I2P, I4P, I8P, &
                                     R4P, R8P, R16P
          IMPLICIT NONE
          private

          PUBLIC :: Cast_To_String
          PUBLIC :: Cast_To_I2, Cast_To_I4, Cast_To_I8
          PUBLIC :: Cast_To_R4, Cast_To_R8, Cast_To_R16
          PUBLIC :: Cast_To_C4, Cast_To_C8, Cast_To_C16

          PUBLIC :: String_Cast
          PUBLIC :: Integer_Cast 
          PUBLIC :: Real_Cast 
          PUBLIC :: Complex_Cast 


          INTERFACE INTEGER_CAST
          MODULE PROCEDURE Int2_Cast, Int4_Cast, Int8_Cast
          END INTERFACE INTEGER_CAST

          INTERFACE REAL_CAST
          MODULE PROCEDURE R4_Cast, R8_Cast, R16_Cast
          END INTERFACE REAL_CAST

          INTERFACE COMPLEX_CAST
          MODULE PROCEDURE C4_Cast, C8_Cast, C16_Cast
          END INTERFACE COMPLEX_CAST


          CONTAINS

!***********************************************************************
        PURE FUNCTION Cast_To_String ( N, Buffer )
!***********************************************************************
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: N                    ! No of Character bytes.
        INTEGER(I1P), DIMENSION(N), INTENT(IN) :: Buffer ! String content
        CHARACTER(N), POINTER  :: Cast_To_String
!-----------------------------------------------------------------------

        IF( ASSOCIATED( Cast_To_String ) ) NULLIFY( Cast_To_String )
        ALLOCATE( Cast_To_String )
        Cast_To_String = TRANSFER( Buffer, Cast_To_String )

!=======================================================================
        END FUNCTION Cast_To_String
!=======================================================================

!***********************************************************************
        PURE FUNCTION Cast_To_I2 ( Buffer )
!***********************************************************************
        IMPLICIT NONE
        INTEGER(I1P), DIMENSION(2), INTENT(IN) :: Buffer ! Integer(2) content
        INTEGER(I2P) :: Cast_To_I2
!-----------------------------------------------------------------------

        Cast_To_I2 = TRANSFER( Buffer, Cast_To_I2 )

!=======================================================================
        END FUNCTION Cast_To_I2
!=======================================================================

!***********************************************************************
        PURE FUNCTION Cast_To_I4 ( Buffer )
!***********************************************************************
        IMPLICIT NONE
        INTEGER(I1P), DIMENSION(4), INTENT(IN) :: Buffer ! Integer(2) content
        INTEGER(I4P) :: Cast_To_I4
!-----------------------------------------------------------------------

        Cast_To_I4 = TRANSFER( Buffer, Cast_To_I4 )

!=======================================================================
        END FUNCTION Cast_To_I4
!=======================================================================

!***********************************************************************
        PURE FUNCTION Cast_To_I8 ( Buffer )
!***********************************************************************
        IMPLICIT NONE
        INTEGER(I1P), DIMENSION(8), INTENT(IN) :: Buffer ! Integer(2) content
        INTEGER(I8P) :: Cast_To_I8
!-----------------------------------------------------------------------

        Cast_To_I8 = TRANSFER( Buffer, Cast_To_I8 )

!=======================================================================
        END FUNCTION Cast_To_I8
!=======================================================================

!***********************************************************************
        PURE FUNCTION Cast_To_R4 ( Buffer )
!***********************************************************************
        IMPLICIT NONE
        INTEGER(I1P), DIMENSION(4), INTENT(IN) :: Buffer ! Integer(2) content
        REAL(R4P) :: Cast_To_R4
!-----------------------------------------------------------------------

        Cast_To_R4 = TRANSFER( Buffer, Cast_To_R4 )

!=======================================================================
        END FUNCTION Cast_To_R4
!=======================================================================

!***********************************************************************
        PURE FUNCTION Cast_To_R8 ( Buffer )
!***********************************************************************
        IMPLICIT NONE
        INTEGER(I1P), DIMENSION(8), INTENT(IN) :: Buffer ! Integer(2) content
        REAL(R8P) :: Cast_To_R8
!-----------------------------------------------------------------------

        Cast_To_R8 = TRANSFER( Buffer, Cast_To_R8 )

!=======================================================================
        END FUNCTION Cast_To_R8
!=======================================================================

!***********************************************************************
        PURE FUNCTION Cast_To_R16 ( Buffer )
!***********************************************************************
        IMPLICIT NONE
        INTEGER(I1P), DIMENSION(16), INTENT(IN) :: Buffer ! Integer(2) content
        REAL(R16P) :: Cast_To_R16
!-----------------------------------------------------------------------

        Cast_To_R16 = TRANSFER( Buffer, Cast_To_R16 )

!=======================================================================
        END FUNCTION Cast_To_R16
!=======================================================================

!***********************************************************************
        PURE FUNCTION Cast_To_C4 ( Buffer )
!***********************************************************************
        IMPLICIT NONE
        INTEGER(I1P), DIMENSION(8), INTENT(IN) :: Buffer ! Integer(2) content
        COMPLEX(R4P) :: Cast_To_C4
!-----------------------------------------------------------------------

        Cast_To_C4 = TRANSFER( Buffer, Cast_To_C4 )

!=======================================================================
        END FUNCTION Cast_To_C4
!=======================================================================

!***********************************************************************
        PURE FUNCTION Cast_To_C8 ( Buffer )
!***********************************************************************
        IMPLICIT NONE
        INTEGER(I1P), DIMENSION(16), INTENT(IN) :: Buffer ! Integer(2) content
        COMPLEX(R8P) :: Cast_To_C8
!-----------------------------------------------------------------------

        Cast_To_C8 = TRANSFER( Buffer, Cast_To_C8 )

!=======================================================================
        END FUNCTION Cast_To_C8
!=======================================================================

!***********************************************************************
        PURE FUNCTION Cast_To_C16 ( Buffer )
!***********************************************************************
        IMPLICIT NONE
        INTEGER(I1P), DIMENSION(32), INTENT(IN) :: Buffer ! Integer(2) content
        COMPLEX(R16P) :: Cast_To_C16
!-----------------------------------------------------------------------

        Cast_To_C16 = TRANSFER( Buffer, Cast_To_C16 )

!=======================================================================
        END FUNCTION Cast_To_C16
!=======================================================================

!***********************************************************************
        PURE SUBROUTINE String_Cast ( Str, Buffer )
!***********************************************************************
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT( IN ) :: Str     ! string to be casted to Integer(1)
        INTEGER(I1P), DIMENSION(:), INTENT( IN OUT ) :: Buffer ! Integer(1) storage
!-----------------------------------------------------------------------
!   NOTE: NO CHECK IS MADE; USER MUST SUPPLY SIZE( Buffer ) = LEN( STR )
!         OTHERWISE AGREE TO ACCEPT BIT TRANCATION ERROR WHICH IS  DANGEROUS !!
!-----------------------------------------------------------------------

        Buffer = TRANSFER( Str, Buffer )

!=======================================================================
        END SUBROUTINE String_Cast
!=======================================================================

!***********************************************************************
        PURE SUBROUTINE Int2_Cast ( Val, Buffer )
!***********************************************************************
        IMPLICIT NONE
        INTEGER(I2P), INTENT( IN ) :: Val     ! INTEGER(2) value to be casted to Integer(1)
        INTEGER(I1P), DIMENSION(2), INTENT( IN OUT ) :: Buffer ! Integer(1) storage
!-----------------------------------------------------------------------

        Buffer = TRANSFER( Val, Buffer )

!=======================================================================
        END SUBROUTINE Int2_Cast
!=======================================================================

!***********************************************************************
        PURE SUBROUTINE Int4_Cast ( Val, Buffer )
!***********************************************************************
        IMPLICIT NONE
        INTEGER(I4P), INTENT( IN ) :: Val     ! INTEGER(4) value to be casted to Integer(1)
        INTEGER(I1P), DIMENSION(4), INTENT( IN OUT ) :: Buffer ! Integer(1) storage
!-----------------------------------------------------------------------

        Buffer = TRANSFER( Val, Buffer )

!=======================================================================
        END SUBROUTINE Int4_Cast
!=======================================================================

!***********************************************************************
        PURE SUBROUTINE Int8_Cast ( Val, Buffer )
!***********************************************************************
        IMPLICIT NONE
        INTEGER(I8P), INTENT( IN ) :: Val     ! INTEGER(8) value to be casted to Integer(1)
        INTEGER(I1P), DIMENSION(8), INTENT( IN OUT ) :: Buffer ! Integer(1) storage
!-----------------------------------------------------------------------

        Buffer = TRANSFER( Val, Buffer )

!=======================================================================
        END SUBROUTINE Int8_Cast
!=======================================================================

!***********************************************************************
        PURE SUBROUTINE R4_Cast ( Val, Buffer )
!***********************************************************************
        IMPLICIT NONE
        REAL(R4P), INTENT( IN ) :: Val     ! REAL(4) value to be casted to Integer(1)
        INTEGER(I1P), DIMENSION(4), INTENT( IN OUT ) :: Buffer ! Integer(1) storage
!-----------------------------------------------------------------------

        Buffer = TRANSFER( Val, Buffer )

!=======================================================================
        END SUBROUTINE R4_Cast
!=======================================================================

!***********************************************************************
        PURE SUBROUTINE R8_Cast ( Val, Buffer )
!***********************************************************************
        IMPLICIT NONE
        REAL(R8P), INTENT( IN ) :: Val     ! REAL(8) value to be casted to Integer(1)
        INTEGER(I1P), DIMENSION(8), INTENT( IN OUT ) :: Buffer ! Integer(1) storage
!-----------------------------------------------------------------------

        Buffer = TRANSFER( Val, Buffer )

!=======================================================================
        END SUBROUTINE R8_Cast
!=======================================================================

!***********************************************************************
        PURE SUBROUTINE R16_Cast ( Val, Buffer )
!***********************************************************************
        IMPLICIT NONE
        REAL(R16P), INTENT( IN ) :: Val     ! REAL(16) value to be casted to Integer(1)
        INTEGER(I1P), DIMENSION(16), INTENT( IN OUT ) :: Buffer ! Integer(1) storage
!-----------------------------------------------------------------------

        Buffer = TRANSFER( Val, Buffer )

!=======================================================================
        END SUBROUTINE R16_Cast
!=======================================================================

!***********************************************************************
        PURE SUBROUTINE C4_Cast ( Val, Buffer )
!***********************************************************************
        IMPLICIT NONE
        COMPLEX(R4P), INTENT( IN ) :: Val     ! COMPLEX(4) value to be casted to Integer(1)
        INTEGER(I1P), DIMENSION(4), INTENT( IN OUT ) :: Buffer ! Integer(1) storage
!-----------------------------------------------------------------------

        Buffer = TRANSFER( Val, Buffer )

!=======================================================================
        END SUBROUTINE C4_Cast
!=======================================================================

!***********************************************************************
        PURE SUBROUTINE C8_Cast ( Val, Buffer )
!***********************************************************************
        IMPLICIT NONE
        COMPLEX(R8P), INTENT( IN ) :: Val     ! COMPLEX(8) value to be casted to Integer(1)
        INTEGER(I1P), DIMENSION(8), INTENT( IN OUT ) :: Buffer ! Integer(1) storage
!-----------------------------------------------------------------------

        Buffer = TRANSFER( Val, Buffer )

!=======================================================================
        END SUBROUTINE C8_Cast
!=======================================================================

!***********************************************************************
        PURE SUBROUTINE C16_Cast ( Val, Buffer )
!***********************************************************************
        IMPLICIT NONE
        COMPLEX(R16P), INTENT( IN ) :: Val     ! COMPLEX(16) value to be casted to Integer(1)
        INTEGER(I1P), DIMENSION(16), INTENT( IN OUT ) :: Buffer ! Integer(1) storage
!-----------------------------------------------------------------------

        Buffer = TRANSFER( Val, Buffer )

!=======================================================================
        END SUBROUTINE C16_Cast
!=======================================================================


!************************************************************************          
          END Module Type_Conversion
!=======================================================================
