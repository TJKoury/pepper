!> This file contains public, global parameter declarations for the SPICELIB Geometry Finder (GF) subsystem.
!>
!>## Restrictions
!>
!>     None.
!>
!>## Author and Institution
!>
!>     N.J. Bachman      (JPL)
!>     L.E. Elson        (JPL)
!>     E.D. Wright       (JPL)
!>
!>## Literature References
!>
!>     None.
!>
!>## Version
!>
!>-    SPICELIB Version 2.0.0  29-NOV-2016 (NJB)
!>
!>        Upgraded to support surfaces represented by DSKs.
!>
!>        Bug fix: removed declaration of NVRMAX parameter.
!>
!>-    SPICELIB Version 1.3.0, 01-OCT-2011 (NJB)
!>
!>       Added NWILUM parameter.
!>
!>-    SPICELIB Version 1.2.0, 14-SEP-2010 (EDW)
!>
!>       Added NWPA parameter.
!>
!>-    SPICELIB Version 1.1.0, 08-SEP-2009 (EDW)
!>
!>       Added NWRR parameter.
!>       Added NWUDS parameter.
!>
!>-    SPICELIB Version 1.0.0, 21-FEB-2009 (NJB) (LSE) (EDW)
module pepper_inc_gf

   use pepper_kinds, only: ip, wp

   implicit none

   public

!
!     Root finding parameters:
!
!     CNVTOL is the default convergence tolerance used by the
!     high-level GF search API routines. This tolerance is
!     used to terminate searches for binary state transitions:
!     when the time at which a transition occurs is bracketed
!     by two times that differ by no more than CNVTOL, the
!     transition time is considered to have been found.
!
!     Units are TDB seconds.
!
   real(wp), parameter :: cnvtol = 1.0e-6_wp

!
!     NWMAX is the maximum number of windows allowed for user-defined
!     workspace array.
!
!        DOUBLE PRECISION      WORK   ( LBCELL : MW, NWMAX )
!
!     Currently no more than twelve windows are required; the three
!     extra windows are spares.
!
!     Callers of GFEVNT can include this file and use the parameter
!     NWMAX to declare the second dimension of the workspace array
!     if necessary.
!
   integer(ip), parameter :: nwmax = 15

!
!     Callers of GFIDST should declare their workspace window
!     count using NWDIST.
!
   integer(ip), parameter :: nwdist = 5

!
!     Callers of GFSEP should declare their workspace window
!     count using NWSEP.
!
   integer(ip), parameter :: nwsep = 5

!
!     Callers of GFRR should declare their workspace window
!     count using NWRR.
!
   integer(ip), parameter :: nwrr = 5

!
!     Callers of GFUDS should declare their workspace window
!     count using NWUDS.
!
   integer(ip), parameter :: nwuds = 5

!
!     Callers of GFPA should declare their workspace window
!     count using NWPA.
!
   integer(ip), parameter :: nwpa = 5

!
!     Callers of GFILUM should declare their workspace window
!     count using NWILUM.
!
   integer(ip), parameter :: nwilum = 5

!
!     ADDWIN is a parameter used to expand each interval of the search
!     (confinement) window by a small amount at both ends in order to
!     accommodate searches using equality constraints. The loaded
!     kernel files must accommodate these expanded time intervals.
!
   real(wp), parameter :: addwin = 0.5_wp

!
!     FRMNLN is a string length for frame names.
!

   integer(ip), parameter :: frmnln = 32

!
!     FOVTLN -- maximum length for FOV string.
!
   integer(ip), parameter :: fovtln = 40

!
!     Specify the character strings that are allowed in the
!     specification of field of view shapes.
!

   character(*), parameter :: ftcirc = 'CIRCLE'

   character(*), parameter :: ftelli = 'ELLIPSE'

   character(*), parameter :: ftpoly = 'POLYGON'

   character(*), parameter :: ftrect = 'RECTANGLE'

!
!     Character strings that are allowed in the
!     specification of occultation types:
!
   character(*), parameter :: annulr = 'ANNULAR'

   character(*), parameter :: any = 'ANY'

   character(*), parameter :: partl = 'PARTIAL'

   character(*), parameter :: full = 'FULL'

!
!     Occultation target shape specifications:
!
   character(*), parameter :: dsshap = 'DSK'

   character(*), parameter :: edshap = 'ELLIPSOID'

   character(*), parameter :: ptshap = 'POINT'

   character(*), parameter :: ryshap = 'RAY'

   character(*), parameter :: spshap = 'SPHERE'

!
!     Specify the number of supported occultation types and occultation
!     type string length:
!
   integer(ip), parameter :: noctyp = 4

   integer(ip), parameter :: oclln = 7

   integer(ip), parameter :: shplen = 9

!
!     Instrument field-of-view (FOV) parameters
!
!     Maximum number of FOV boundary vectors:
!
   integer(ip), parameter :: maxvrt = 10000

!
!     FOV shape parameters:
!
!        circle
!        ellipse
!        polygon
!        rectangle
!
   character(*), parameter :: cirfov = 'CIRCLE'

   character(*), parameter :: ellfov = 'ELLIPSE'

   character(*), parameter :: polfov = 'POLYGON'

   character(*), parameter :: recfov = 'RECTANGLE'

end module pepper_inc_gf
