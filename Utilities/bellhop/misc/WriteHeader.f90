SUBROUTINE WriteHeader( FileName, Title, theta, Ntheta, sd, Nsd, rd, Nrd, r, Nr, Freq, Atten, PlotType, XS, YS )

  ! Write header to disk file

  ! FileName is usually SHDFIL for complex pressure or GRNFIL for a Green's function
  ! Title  is an arbitrary title
  ! theta  is a vector of bearing lines,   theta( 1: Ntheta )
  ! sd     is a vector of source   depths, sd(1:Nsd)
  ! rd     is a vector of receiver depths, rd(1:Nrd)
  ! r      is a vector of receiver ranges, r(1:Nr)
  ! Freq   is the frequency
  ! Atten  is the stabilizing attenuation (which is only
  !    important for FFP runs. Use zero for other cases.)

  IMPLICIT NONE
  INTEGER, PARAMETER :: SHDFil = 25
  INTEGER Ntheta, Nsd, Nrd, Nr, LRecl
  REAL (KIND=4) :: theta( * ), r( * ), rd( * ), sd( * ), Freq, XS, YS, Atten
  CHARACTER FileName*( 6 ), Title*( * ), PlotType*( 10 )

  LRecl = MAX( 40, Ntheta, Nsd, Nrd, 2 * Nr )   ! words/record

  OPEN ( FILE = FileName, UNIT = SHDFil, STATUS = 'UNKNOWN', ACCESS = 'DIRECT', RECL = 4 * LRecl, FORM = 'UNFORMATTED')
  WRITE( SHDFil, REC = 1 ) LRecl, Title(1:80)
  WRITE( SHDFil, REC = 2 ) PlotType, XS, YS
  WRITE( SHDFil, REC = 3 ) Freq, Ntheta, Nsd, Nrd, Nr, atten
  WRITE( SHDFil, REC = 4 ) theta( 1 : Ntheta )
  WRITE( SHDFil, REC = 5 ) sd( 1 : Nsd )
  WRITE( SHDFil, REC = 6 ) rd( 1 : Nrd )
  WRITE( SHDFil, REC = 7 ) r(  1 : Nr  )

  RETURN
END SUBROUTINE WriteHeader

!**********************************************************************!

SUBROUTINE WRTFLD( P, Nrd, Nr, IRec )

  ! Write the field to disk

  INTEGER, PARAMETER :: SHDFil = 25
  COMPLEX P( Nrd, * )

  DO I = 1, Nrd
     IRec = IRec + 1
     WRITE( SHDFil, REC = IRec ) P( I, 1 : Nr )
  END DO

  RETURN
END SUBROUTINE WRTFLD
