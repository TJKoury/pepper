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
      DOUBLE PRECISION      CNVTOL
      PARAMETER           ( CNVTOL = 1.D-6 )


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
      INTEGER               NWMAX
      PARAMETER           ( NWMAX = 15 )

!
!     Callers of GFIDST should declare their workspace window
!     count using NWDIST.
!
      INTEGER               NWDIST
      PARAMETER           ( NWDIST = 5 )


!
!     Callers of GFSEP should declare their workspace window
!     count using NWSEP.
!
      INTEGER               NWSEP
      PARAMETER           ( NWSEP  = 5 )


!
!     Callers of GFRR should declare their workspace window
!     count using NWRR.
!
      INTEGER               NWRR
      PARAMETER           ( NWRR   = 5 )


!
!     Callers of GFUDS should declare their workspace window
!     count using NWUDS.
!
      INTEGER               NWUDS
      PARAMETER           ( NWUDS  = 5 )


!
!     Callers of GFPA should declare their workspace window
!     count using NWPA.
!
      INTEGER               NWPA
      PARAMETER           ( NWPA  = 5 )

!
!     Callers of GFILUM should declare their workspace window
!     count using NWILUM.
!
      INTEGER               NWILUM
      PARAMETER           ( NWILUM = 5 )


!
!     ADDWIN is a parameter used to expand each interval of the search
!     (confinement) window by a small amount at both ends in order to
!     accommodate searches using equality constraints. The loaded
!     kernel files must accommodate these expanded time intervals.
!
      DOUBLE PRECISION      ADDWIN
      PARAMETER           ( ADDWIN = 0.5D0 )

!
!     FRMNLN is a string length for frame names.
!

      INTEGER               FRMNLN
      PARAMETER           ( FRMNLN = 32 )

!
!     FOVTLN -- maximum length for FOV string.
!
      INTEGER               FOVTLN
      PARAMETER           ( FOVTLN = 40 )

!
!     Specify the character strings that are allowed in the
!     specification of field of view shapes.
!


      CHARACTER*(*)         FTCIRC
      PARAMETER           ( FTCIRC = 'CIRCLE' )

      CHARACTER*(*)         FTELLI
      PARAMETER           ( FTELLI = 'ELLIPSE' )

      CHARACTER*(*)         FTPOLY
      PARAMETER           ( FTPOLY = 'POLYGON' )

      CHARACTER*(*)         FTRECT
      PARAMETER           ( FTRECT = 'RECTANGLE' )

!
!     Character strings that are allowed in the
!     specification of occultation types:
!
      CHARACTER*(*)         ANNULR
      PARAMETER           ( ANNULR = 'ANNULAR' )

      CHARACTER*(*)         ANY
      PARAMETER           ( ANY    = 'ANY'     )

      CHARACTER*(*)         PARTL
      PARAMETER           ( PARTL  = 'PARTIAL' )

      CHARACTER*(*)         FULL
      PARAMETER           ( FULL   = 'FULL'    )

!
!     Occultation target shape specifications:
!
      CHARACTER*(*)         DSSHAP
      PARAMETER           ( DSSHAP = 'DSK' )

      CHARACTER*(*)         EDSHAP
      PARAMETER           ( EDSHAP = 'ELLIPSOID' )

      CHARACTER*(*)         PTSHAP
      PARAMETER           ( PTSHAP = 'POINT' )

      CHARACTER*(*)         RYSHAP
      PARAMETER           ( RYSHAP = 'RAY' )

      CHARACTER*(*)         SPSHAP
      PARAMETER           ( SPSHAP = 'SPHERE' )

!
!     Specify the number of supported occultation types and occultation
!     type string length:
!
      INTEGER               NOCTYP
      PARAMETER           ( NOCTYP = 4 )

      INTEGER               OCLLN
      PARAMETER           ( OCLLN  = 7 )

      INTEGER               SHPLEN
      PARAMETER           ( SHPLEN = 9 )

!
!     Instrument field-of-view (FOV) parameters
!
!     Maximum number of FOV boundary vectors:
!
      INTEGER               MAXVRT
      PARAMETER           ( MAXVRT = 10000 )

!
!     FOV shape parameters:
!
!        circle
!        ellipse
!        polygon
!        rectangle
!
      CHARACTER*(*)         CIRFOV
      PARAMETER           ( CIRFOV = 'CIRCLE' )

      CHARACTER*(*)         ELLFOV
      PARAMETER           ( ELLFOV = 'ELLIPSE' )

      CHARACTER*(*)         POLFOV
      PARAMETER           ( POLFOV = 'POLYGON' )

      CHARACTER*(*)         RECFOV
      PARAMETER           ( RECFOV = 'RECTANGLE' )


!
!     End of file gf.inc.
!
