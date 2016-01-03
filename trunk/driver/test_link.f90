          program main
          use My_Datatypes
          use CLASS_G_LIST
          implicit none
          integer :: i,j,k
          real(dp) :: a,b,c
          complex(dp) :: ai,bi,ci
          character(max_name_len) :: str1,str2,str3
          type( G_Link ) :: link
          type( G_List ) :: list

          ai = cmplx(10.0d0, 0.0d0)
          bi = cmplx(11.0d0, -1.0d0)
          ci = cmplx(3.0d0, 12.0d0)
          call list%erase()
          call list%push_back("a1","jahangir")
          call list%push_back("b2",(/1,2,3/))
          call list%push_back("c3",(/10.0d0,11.0d0,12.0d0/))
          call list%push_back("d4",(/ ai, bi, ci/))
          call list%print_list()

          end program main

           


