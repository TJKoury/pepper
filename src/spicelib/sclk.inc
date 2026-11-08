! $ Abstract
!
!     Include file sclk.inc
!
!     SPICE private file intended solely for the support of SPICE
!     routines.  Users should not include this file directly due
!     to the volatile nature of this file
!
!     The parameters below define sizes and limits used by
!     the SCLK system.
!
! $ Parameters
!
!     See the declaration section below.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 20-OCT-2020 (NJB)
!
!        Increased MXCOEF to 100000.
!
!        Updated comments with reminder to keep constants declared
!        in the include file zzsc01.inc synced with constants in
!        this file.
!
!-    SPICELIB Version 2.0.0, 24-MAY-2010 (NJB)
!
!        Increased value of maximum coefficient record count
!        parameter MXCOEF from 10K to 50K.
!
!-    SPICELIB Version 1.0.0, 11-FEB-2008 (NJB)
!
!-&

!
!        NOTE: many of the declarations present here are duplicated
!        in the include file zzsc01.inc. Declarations in that file
!        must be kept in sync with those in this file. The
!        duplicated declarations are:
!
!           NDELIM
!           DELIMS
!           MXPART
!           MXCOEF
!           MXNFLD
!           DPLEN
!
!
!     Number of supported SCLK field delimiters:
!
      INTEGER               NDELIM
      PARAMETER           ( NDELIM = 5 )

!
!     Supported SCLK string field delimiters:
!
      CHARACTER*(NDELIM)    DELIMS
      PARAMETER           ( DELIMS = '.:-, ' )

!
!     Maximum number of partitions:
!
      INTEGER               MXPART
      PARAMETER           ( MXPART = 9999 )

!
!     Partition string length.
!
!     Since the maximum number of partitions is given by MXPART is
!     9999, PRTSTR needs at most 4 characters for the partition number
!     and one character for the slash.
!
      INTEGER               PARTLN
      PARAMETER           ( PARTLN = 5 )

!
!     Maximum number of coefficient records:
!
      INTEGER               MXCOEF
      PARAMETER           ( MXCOEF = 100000 )

!
!     Maximum number of fields in an SCLK string:
!
      INTEGER               MXNFLD
      PARAMETER           ( MXNFLD = 10 )

!
!     Length of strings used to represent D.P.
!     numbers:
!
      INTEGER               DPLEN
      PARAMETER           ( DPLEN = 30 )

!
!     Maximum number of supported parallel time systems:
!
      INTEGER               MXTSYS
      PARAMETER           ( MXTSYS = 2 )

!
!     End of include file sclk.inc
!
