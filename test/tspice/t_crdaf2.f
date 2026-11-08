C$Procedure  T_CRDAF2 ( Create Supporting DAF Files )

      SUBROUTINE T_CRDAF2 ( TYPE, NAME, IFNAME, NSEG, IDS, TBEGS, 
     .                                             TENDS, SEGIDS )

C$ Abstract
C
C     Create a new DAF with specifed segments and user-provided 
C     internal file name.
C
C        07-NOV-2001 (NJB)
C        26-APR-2012 (BVS)
C
C     Support routine for TSPICE test families 
C 
C       f_pckbsr
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

      CHARACTER*(*)         TYPE
      CHARACTER*(*)         NAME
      CHARACTER*(*)         IFNAME
      INTEGER               NSEG
      INTEGER               IDS    ( * )
      DOUBLE PRECISION      TBEGS  ( * )
      DOUBLE PRECISION      TENDS  ( * )
      CHARACTER*(*)         SEGIDS ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TYPE       I   'SPK', 'CK', or 'PCK'.
C     NAME       I   File name.
C     IFNAME     I   Internal file name.
C     NSEG       I   Number of segments.
C     IDS        I   Body IDs.
C     TBEGS      I   Start times.
C     TENDS      I   Stop times.
C     SEGIDS     I   Segment IDs.
C     
C$ Detailed_Input
C
C     See Brief_I/O.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     Some routines in the call tree of this routine may signal errors.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine creates SPK, CK, or binary PCK file to support
C     testing.
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
C     B.V. Semenov     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 26-APR-2012 (NJB) (BVS)
C
C        Based on T_CRDAF.
C
C-&

C
C     SPICELIB functions
C

C
C     Local parameters
C
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 3 )

C
C     Local variables
C
      CHARACTER*(TYPLEN)    LOCTYP

      DOUBLE PRECISION      DESCR  ( DSCSIZ )
      
      INTEGER               HANDLE
      INTEGER               I


      CALL CHKIN ( 'T_CRDAF2' )

      CALL LJUST ( TYPE,   LOCTYP )
      CALL UCASE ( LOCTYP, LOCTYP )

      IF ( LOCTYP .EQ. 'SPK' )  THEN
         
         CALL SPKOPN ( NAME, IFNAME, 0, HANDLE )

      ELSE IF (  LOCTYP .EQ.  'CK' )  THEN 

         CALL CKOPN  ( NAME, IFNAME, 0, HANDLE )

      ELSE

         CALL PCKOPN ( NAME, IFNAME, 0, HANDLE )

      END IF


      DO I = 1, NSEG

         CALL T_CRDESC ( LOCTYP, I, IDS(I), TBEGS(I), TENDS(I), DESCR )

         CALL DAFBNA ( HANDLE, DESCR, SEGIDS(I) )
         CALL DAFENA

      END DO

      CALL DAFCLS ( HANDLE )

      CALL CHKOUT ( 'T_CRDAF2' )
      END

