C$Procedure      F_ZZDSKSEL ( Test the entry points of ZZDSKSEL )
 
      SUBROUTINE F_ZZDSKSEL ( OK )
  
C$ Abstract
C
C     Perform a collection of rudimentary tests on the
C     ZZDSKSEL collection of entry points.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'dla.inc'
      INCLUDE 'dsk.inc'
      INCLUDE 'dskdsc.inc'


      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The umbrella routine ZZDSKSEL contains the following
C     entry points:
C     
C
C     listed below.
C
C         Match body ID only
C         ------------------
C
C           Initialization:       ZZDSKSBD
C           Segment comparison:   ZZDSKBDC
C
C
C     The remaining entry points are included to support the SMAP
C     Alpha DSK Toolkit. They may be useful for DSK type 4 in a
C     future SPICELIB version.
C
C
C         Match body ID, surface list, and time
C         -------------------------------------
C
C           Initialization:       ZZDSKSIT
C           Segment comparison:   ZZDSKCIT
C
C
C         Match body ID, time, and coordinates
C         ------------------------------------
C
C           Initialization:       ZZDSKUSC
C           Segment comparison:   ZZDSKUMC
C
C
C         Match body ID, surface ID, frame ID, coordinate system,
C         coordinate parameters, time, and coordinates
C         ------------------------------------------------------
C
C           Initialization:       ZZDSKMSC
C           Segment comparison:   ZZDSKMMC
C
C
C         Match body ID, surface ID, frame ID, data class, time,
C         and whether coverage includes a specified position vector
C         ---------------------------------------------------------
C
C           Initialization:       ZZDSKSRC
C           Segment comparison:   ZZDSKMRC
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0 20-MAY-2016 (NJB)
C
C-&
 
  
  
C
C     SPICELIB functions
C
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      RPD
 
      LOGICAL               ZZDSKBDC
      LOGICAL               ZZDSKCIT
      LOGICAL               ZZDSKSBD
      LOGICAL               ZZDSKSEL
      LOGICAL               ZZDSKSIT
      LOGICAL               ZZDSKUMC
      LOGICAL               ZZDSKUSC

C
C     Local parameters
C 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
      
C
C     Local variables
C     
      DOUBLE PRECISION      BTIME
      DOUBLE PRECISION      COR1
      DOUBLE PRECISION      COR2
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETIME
      DOUBLE PRECISION      POS    ( 3 )

      INTEGER               BODYID
      INTEGER               CORSYS
      INTEGER               DCLASS
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               FRAMID
      INTEGER               HANDLE
      INTEGER               NSURF
      INTEGER               SURFID
      INTEGER               SRFLST ( MAXSRF )

      LOGICAL               ISMTCH
      LOGICAL               LGRETV

C
C     Saved variables
C
      SAVE




C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZDSKSEL' )


C***********************************************************************
C
C     ZZDSKSEL umbrella tests
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Direct call to ZZDSKSEL' )

      LGRETV = ZZDSKSEL ( SURFID, NSURF,  SRFLST,
     .                    BODYID, DCLASS, CORSYS, CORPAR,
     .                    COR1,   COR2,   FRAMID, POS,  
     .                    ET,     HANDLE, DLADSC, DSKDSC )

      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )


C***********************************************************************
C
C     ZZDSKSBD/ZZDSKBDC tests
C
C***********************************************************************
 
C***********************************************************************
C
C     ZZDSKSBD/ZZDSKBDC normal cases
C
C***********************************************************************

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: match case' )
 
C
C     Initialize handle and DLA descriptor. Neither are
C     used for this comparison.
C
      HANDLE = 0
      CALL CLEARI ( DLADSZ, DLADSC )


      BODYID = 599
      
      LGRETV = ZZDSKSBD( BODYID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The DSK descriptor must have a matching body ID code.
C
      CALL CLEARD ( DSKDSZ, DSKDSC )

C
C     The body ID index in a DSK descriptor is called 'CTRIDX'.
C
      DSKDSC( CTRIDX ) = DBLE( BODYID )


      ISMTCH =  ZZDSKBDC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .TRUE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: non-match case' )
 

      DSKDSC( CTRIDX ) = DBLE( BODYID + 1 ) 

      ISMTCH =  ZZDSKBDC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )

C
C     No error cases exist for these routines.
C 


C***********************************************************************
C
C     ZZDSKSIT/ZZDSKCIT tests
C
C***********************************************************************
 
C***********************************************************************
C
C     ZZDSKSIT/ZZDSKCIT normal cases
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: match case, empty '
     .//           'surface list.' )
 
C
C     Initialize handle and DLA descriptor. Neither are
C     used for this comparison.
C
      HANDLE = 0
      CALL CLEARI ( DLADSZ, DLADSC )


      BODYID = 599
      NSURF  = 0
      
      CALL CLEARI ( MAXSRF, SRFLST )

      ET = 10.D0

      LGRETV = ZZDSKSIT( BODYID, NSURF, SRFLST, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The DSK descriptor must have a matching body ID code.
C
      CALL CLEARD ( DSKDSZ, DSKDSC )

      DSKDSC( CTRIDX ) = DBLE( BODYID )

C
C     Set time bounds. The bounds must bracket ET.
C
      BTIME            = -50*JYEAR()
      ETIME            =  50*JYEAR()

      DSKDSC( BTMIDX ) = BTIME
      DSKDSC( ETMIDX ) = ETIME

C
C     The body ID index in a DSK descriptor is called 'CTRIDX'.
C

      ISMTCH =  ZZDSKCIT ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .TRUE., OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: match case, non-empty '
     .//           'surface list.' )
 
C
C     Initialize handle and DLA descriptor. Neither are
C     used for this comparison.
C
      HANDLE = 0
      CALL CLEARI ( DLADSZ, DLADSC )


      BODYID    = 599

      CALL CLEARI ( MAXSRF, SRFLST )
      NSURF     = 2
      SRFLST(1) = -1
      SRFLST(2) = -2
      

      ET        = 10.D0

      LGRETV = ZZDSKSIT( BODYID, NSURF, SRFLST, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     The DSK descriptor must have a matching body ID code.
C
      CALL CLEARD ( DSKDSZ, DSKDSC )

C
C     Set the surface ID code.
C
      DSKDSC( SRFIDX ) = SRFLST(1)

C
C     Set time bounds. The bounds must bracket ET.
C
      DSKDSC( CTRIDX ) = DBLE( BODYID )

      BTIME            = -50*JYEAR()
      ETIME            =  50*JYEAR()

      DSKDSC( BTMIDX ) = BTIME
      DSKDSC( ETMIDX ) = ETIME

C
C     Look for a match.
C 
      ISMTCH =  ZZDSKCIT ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH (-1)', ISMTCH, .TRUE., OK )

C
C     Try again with the other surface.
C
      DSKDSC( SRFIDX ) = SRFLST(2)
      

      ISMTCH =  ZZDSKCIT ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH (-2)', ISMTCH, .TRUE., OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: non-match case, body '
     .//           'mismatch.' )
 
      
C
C     Save body ID.
C
      BODYID = NINT( DSKDSC(CTRIDX) )

      DSKDSC( CTRIDX ) = 0


      ISMTCH =  ZZDSKCIT ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )
C
C     Restore body ID.
C     
      DSKDSC( CTRIDX ) = DBLE( BODYID )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: non-match case, time '
     .//           'too early.' )
       

      LGRETV = ZZDSKSIT( BODYID, NSURF, SRFLST, BTIME-1.D0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKCIT ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: non-match case, time '
     .//           'too late.' )
       

      LGRETV = ZZDSKSIT( BODYID, NSURF, SRFLST, ETIME+1.D0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKCIT ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )
 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: non-match case, surface '
     .//           'not in list.' )
       
      

      LGRETV = ZZDSKSIT( BODYID, 1, -3, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKCIT ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )
 

C***********************************************************************
C
C     ZZDSKSIT/ZZDSKCIT error cases
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSBD/ZZDSKBDC: surface list too big.' )
 
      LGRETV = ZZDSKSIT( BODYID, MAXSRF+1, SRFLST, ET )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK ) 
     





C***********************************************************************
C
C     ZZDSKUSC/ZZDSKUMC tests
C
C***********************************************************************
 
C***********************************************************************
C
C     ZZDSKUSC/ZZDSKUMC normal cases
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: match case.' )


C
C     Initialize handle and DLA descriptor. Neither are
C     used for this comparison.
C
      HANDLE = 0
      CALL CLEARI ( DLADSZ, DLADSC )


      BODYID = 599
 
      ET     = 10.D0

      COR1   = 30.D0 * RPD()
      COR2   = -5.D0 * RPD()

      LGRETV = ZZDSKUSC( BODYID, ET, COR1, COR2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The DSK descriptor must have a matching body ID code.
C
      CALL CLEARD ( DSKDSZ, DSKDSC )

      DSKDSC( CTRIDX ) = DBLE( BODYID )

C
C     Set time bounds. The bounds must bracket ET.
C
      BTIME            = -50*JYEAR()
      ETIME            =  50*JYEAR()

      DSKDSC( BTMIDX ) = BTIME
      DSKDSC( ETMIDX ) = ETIME

C
C     Set coordinate system and coordinate bounds. 
C
      DSKDSC( SYSIDX ) = DBLE( LATSYS )

      DSKDSC( MN1IDX ) = -90.D0 * RPD()
      DSKDSC( MX1IDX ) =  90.D0 * RPD()

      DSKDSC( MN2IDX ) = -45.D0 * RPD()
      DSKDSC( MX2IDX ) =  45.D0 * RPD()

      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .TRUE., OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: non-match case, body '
     .//           'mismatch.' )
 
      
C
C     Save body ID.
C
      BODYID = NINT( DSKDSC(CTRIDX) )

      DSKDSC( CTRIDX ) = 0


      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )
C
C     Restore body ID.
C     
      DSKDSC( CTRIDX ) = DBLE( BODYID )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: non-match case, time '
     .//           'too early.' )
 

      LGRETV = ZZDSKUSC( BODYID, BTIME-1, COR1, COR2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: non-match case, time '
     .//           'too late.' )
 

      LGRETV = ZZDSKUSC( BODYID, ETIME+1, COR1, COR2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: non-match case, coordinate '
     .//           '1 too small' )
 

      LGRETV = ZZDSKUSC( BODYID, ET, DSKDSC(MN1IDX)-0.1D0, COR2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: non-match case, coordinate '
     .//           '1 too large' )
 

      LGRETV = ZZDSKUSC( BODYID, ET, DSKDSC(MX1IDX)+0.1D0, COR2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: non-match case, coordinate '
     .//           '2 too small' )
 

      LGRETV = ZZDSKUSC( BODYID, ET, COR1, DSKDSC(MN2IDX)-0.1D0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKUSC/ZZDSKUMC: non-match case, coordinate '
     .//           '2 too large' )
 

      LGRETV = ZZDSKUSC( BODYID, ET, COR1, DSKDSC(MX2IDX)+0.1D0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ISMTCH =  ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISMTCH', ISMTCH, .FALSE., OK )

C
C     No error cases exist for these routines.
C 


C***********************************************************************
C
C     ZZDSKMSC/ZZDSKMMC tests
C
C***********************************************************************
 
C***********************************************************************
C
C     ZZDSKMSC/ZZDSKMMC normal cases
C
C***********************************************************************

C
C     TBD
C 




      CALL T_SUCCESS ( OK )
      RETURN
      END
