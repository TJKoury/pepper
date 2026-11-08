C$Procedure F_ZZDSKSGX ( ZZDSKSGX tests )
 
      SUBROUTINE F_ZZDSKSGX ( OK )
 
C$ Abstract
C
C     Exercise the routine ZZDSKSGX.
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
      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsktol.inc'
      
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
C     This routine tests the SPICELIB routine ZZDSKSGX.
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
C-    TSPICE Version 1.0.0, 17-JUN-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               EXISTS

C
C     Local Parameters 
C
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0   = 'zzdsksgx_0.bds' )

      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'zzdsksgx.tpc' )


      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

 
C
C     Local Variables 
C  

      CHARACTER*(NAMLEN)    FRNAME

      DOUBLE PRECISION      D
      DOUBLE PRECISION      DC     ( 1 )
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )

      INTEGER               BODYID
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               DTYPE
      INTEGER               HANDLE
      INTEGER               IC     ( 1 )
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               PLID
      INTEGER               SURFID

      LOGICAL               FOUND


C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C
 


C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZDSKSGX' )



C**********************************************************************
C
C     Normal cases
C
C**********************************************************************


C
C     The following simple cases are meant to assist debugging.
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Set-up: create tessellated ellipsoid DSK.'  )
  

C
C     Create, load, and discard generic test PCK.
C     
      CALL TSTPCK ( PCK0, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create DSK.
C
C     TARGET = 'MARS'
      FRNAME = 'IAU_MARS'
      BODYID = 499
      SURFID = 1

      NLON   = 40
      NLAT   = 20

      IF ( EXISTS(DSK0) ) THEN
         CALL DELFIL ( DSK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL T_ELDS2Z ( BODYID, SURFID, FRNAME, NLON, NLAT, DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Mars segment: check intercept.' )
  

      CALL DASOPR ( DSK0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Descr FOUND', FOUND, .TRUE., OK )
      
      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DTYPE =   NINT( DSKDSC( TYPIDX ) )
      ET    = ( DSKDSC( BTMIDX ) + DSKDSC( ETMIDX ) ) / 2

      D     = 1.D6
      CALL VPACK  ( D, 2*D, 3*D, VERTEX )
      CALL VMINUS ( VERTEX,      RAYDIR )

      CALL ZZDSKSGX ( HANDLE, DLADSC, DTYPE, ET, VERTEX,
     .                RAYDIR, XPT,    DC,    IC, FOUND  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'xpt FOUND', FOUND, .TRUE., OK )

C
C     Compare results to those from DSKX02.
C
      CALL DSKX02 ( HANDLE, DLADSC, VERTEX, RAYDIR, PLID, XXPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'xxpt FOUND', FOUND, .TRUE., OK )

C
C     We expect an exact match for the intercept.
C
      CALL CHCKAD ( 'XPT', XPT, '=', XXPT, 3, 0.D0, OK )

C
C     Check the integer info component. (The d.p. component
C     is undefined.)
C
      CALL CHCKAI ( 'IC', IC, '=', PLID, 1, OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Mars segment: non-intercept case.' )
  
C
C     Just point the ray away from the target.
C
      CALL ZZDSKSGX ( HANDLE, DLADSC, DTYPE, ET, VERTEX,
     .                VERTEX, XPT,    DC,    IC, FOUND  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'xpt FOUND', FOUND, .FALSE., OK )


C**********************************************************************
C
C     Error cases
C
C**********************************************************************



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Invalid file handle'  )

      CALL ZZDSKSGX ( HANDLE+1, DLADSC, DTYPE, ET, VERTEX,
     .                RAYDIR,   XPT,    DC,    IC, FOUND  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Bad data type'  )

      CALL ZZDSKSGX ( HANDLE+1, DLADSC, -1,  ET, VERTEX,
     .                RAYDIR,   XPT,    DC,  IC, FOUND  )
      CALL CHCKXC ( .TRUE., 'SPICE(TYPENOTSUPPORTED)', OK )

  


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Clean up.'  )
  
C
C     Close DSK file; open was performed using DASOPR.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


