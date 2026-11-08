! $ Abstract
!
!     This file contains public, global parameter declarations
!     for the SPICELIB Geometry Finder (GF) subsystem.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     GEOMETRY
!     ROOT
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman      (JPL)
!     L.E. Elson        (JPL)
!     E.D. Wright       (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 2.0.0  29-NOV-2016 (NJB)
!
!        Upgraded to support surfaces represented by DSKs.
!
!        Bug fix: removed declaration of NVRMAX parameter.
!
!-    SPICELIB Version 1.3.0, 01-OCT-2011 (NJB)
!
!       Added NWILUM parameter.
!
!-    SPICELIB Version 1.2.0, 14-SEP-2010 (EDW)
!
!       Added NWPA parameter.
!
!-    SPICELIB Version 1.1.0, 08-SEP-2009 (EDW)
!
!       Added NWRR parameter.
!       Added NWUDS parameter.
!
!-    SPICELIB Version 1.0.0, 21-FEB-2009 (NJB) (LSE) (EDW)
!
!-&


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
      double precision      cnvtol
      parameter           ( cnvtol = 1.d-6 )


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
      integer               nwmax
      parameter           ( nwmax = 15 )

!
!     Callers of GFIDST should declare their workspace window
!     count using NWDIST.
!
      integer               nwdist
      parameter           ( nwdist = 5 )


!
!     Callers of GFSEP should declare their workspace window
!     count using NWSEP.
!
      integer               nwsep
      parameter           ( nwsep  = 5 )


!
!     Callers of GFRR should declare their workspace window
!     count using NWRR.
!
      integer               nwrr
      parameter           ( nwrr   = 5 )


!
!     Callers of GFUDS should declare their workspace window
!     count using NWUDS.
!
      integer               nwuds
      parameter           ( nwuds  = 5 )


!
!     Callers of GFPA should declare their workspace window
!     count using NWPA.
!
      integer               nwpa
      parameter           ( nwpa  = 5 )

!
!     Callers of GFILUM should declare their workspace window
!     count using NWILUM.
!
      integer               nwilum
      parameter           ( nwilum = 5 )


!
!     ADDWIN is a parameter used to expand each interval of the search
!     (confinement) window by a small amount at both ends in order to
!     accommodate searches using equality constraints. The loaded
!     kernel files must accommodate these expanded time intervals.
!
      double precision      addwin
      parameter           ( addwin = 0.5d0 )

!
!     FRMNLN is a string length for frame names.
!

      integer               frmnln
      parameter           ( frmnln = 32 )

!
!     FOVTLN -- maximum length for FOV string.
!
      integer               fovtln
      parameter           ( fovtln = 40 )

!
!     Specify the character strings that are allowed in the
!     specification of field of view shapes.
!


      character*(*)         ftcirc
      parameter           ( ftcirc = 'CIRCLE' )

      character*(*)         ftelli
      parameter           ( ftelli = 'ELLIPSE' )

      character*(*)         ftpoly
      parameter           ( ftpoly = 'POLYGON' )

      character*(*)         ftrect
      parameter           ( ftrect = 'RECTANGLE' )

!
!     Character strings that are allowed in the
!     specification of occultation types:
!
      character*(*)         annulr
      parameter           ( annulr = 'ANNULAR' )

      character*(*)         any
      parameter           ( any    = 'ANY'     )

      character*(*)         partl
      parameter           ( partl  = 'PARTIAL' )

      character*(*)         full
      parameter           ( full   = 'FULL'    )

!
!     Occultation target shape specifications:
!
      character*(*)         dsshap
      parameter           ( dsshap = 'DSK' )

      character*(*)         edshap
      parameter           ( edshap = 'ELLIPSOID' )

      character*(*)         ptshap
      parameter           ( ptshap = 'POINT' )

      character*(*)         ryshap
      parameter           ( ryshap = 'RAY' )

      character*(*)         spshap
      parameter           ( spshap = 'SPHERE' )

!
!     Specify the number of supported occultation types and occultation
!     type string length:
!
      integer               noctyp
      parameter           ( noctyp = 4 )

      integer               oclln
      parameter           ( oclln  = 7 )

      integer               shplen
      parameter           ( shplen = 9 )

!
!     Instrument field-of-view (FOV) parameters
!
!     Maximum number of FOV boundary vectors:
!
      integer               maxvrt
      parameter           ( maxvrt = 10000 )

!
!     FOV shape parameters:
!
!        circle
!        ellipse
!        polygon
!        rectangle
!
      character*(*)         cirfov
      parameter           ( cirfov = 'CIRCLE' )

      character*(*)         ellfov
      parameter           ( ellfov = 'ELLIPSE' )

      character*(*)         polfov
      parameter           ( polfov = 'POLYGON' )

      character*(*)         recfov
      parameter           ( recfov = 'RECTANGLE' )


!
!     End of file gf.inc.
!
