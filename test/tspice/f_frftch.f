C$Procedure      F_FRFTCH ( Family of tests for frame fetch APIs )

      SUBROUTINE F_FRFTCH ( OK )

C$ Abstract
C
C     This test family checks out the frame ID fetch APIs.
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

      INCLUDE 'frmtyp.inc'
      INCLUDE 'ninert.inc'
      INCLUDE 'nninrt.inc'

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
C     trapped or unintentional and need reporting. The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the frame ID code fetch APIs listed below:
C
C        BLTFRM
C        KPLFRM
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
C-    TSPICE Version 3.0.0, 14-OCT-2020 (NJB)
C
C        Updated to account for switch frame class.
C
C-    TSPICE Version 2.0.0, 09-FEB-2016 (NJB)
C
C-       22-JUN-2015 (NJB)
C
C        Updated to exercise KPLFRM buffering logic.
C
C-    TSPICE Version 1.1.0, 09-AUG-2013 (BVS)
C
C        Updated for changed ZZFDAT calling sequence.
C
C-    TSPICE Version 1.0.0, 23-MAY-2012 (NJB)
C
C-&



C
C     Test Utility Functions
C

C
C     SPICELIB Functions
C
      INTEGER               CARDI

C
C     Local parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 5000 )

      INTEGER               MAXBLT
      PARAMETER           ( MAXBLT = NINERT + NNINRT )

      INTEGER               MAXFRM
      PARAMETER           ( MAXFRM = MAXBLT )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 400 )

      INTEGER               NCL
      PARAMETER           ( NCL    = 6 )

C
C     Local Variables
C
      CHARACTER*(FRNMLN)    FRNAME
      CHARACTER*(KVNMLN)    KVNAME
      CHARACTER*(FRNMLN)    NAMES  ( MAXBLT )
      CHARACTER*(LNSIZE)    TITLE

      INTEGER               BIGN
      INTEGER               CENTRD ( MAXBLT )
      INTEGER               CLSIDX
      INTEGER               CTRS   ( MAXBLT )
      INTEGER               FRCLSS
      INTEGER               FRCLID
      INTEGER               FRCODE
      INTEGER               FRCENT
      INTEGER               FRMID
      INTEGER               I
      INTEGER               IDCDES ( MAXBLT )
      INTEGER               IDSET  ( LBCELL : BUFSIZ )
      INTEGER               IDLIST ( BUFSIZ )
      INTEGER               N
      INTEGER               NCLASS ( NCL )
      INTEGER               NCOUNT
      INTEGER               TYPES  ( MAXBLT )
      INTEGER               TYPIDS ( MAXBLT )

      INTEGER               XIDSET ( LBCELL : BUFSIZ )

C
C     Built-in frame hashes returned by ZZFDAT.
C
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               MAXBFR
      PARAMETER           ( MAXBFR = MAXBLT+1 )

      INTEGER               BNMLST  (          MAXBFR )
      INTEGER               BNMPOL  ( LBPOOL : MAXBFR )
      CHARACTER*(FRNMLN)    BNMNMS  (          MAXBFR )
      INTEGER               BNMIDX  (          MAXBFR )

      INTEGER               BIDLST  (          MAXBFR )
      INTEGER               BIDPOL  ( LBPOOL : MAXBFR )
      INTEGER               BIDIDS  (          MAXBFR )
      INTEGER               BIDIDX  (          MAXBFR )

C
C     Saved variables
C
C     Save all variables to avoid CSPICE stack overflow
C     problems.
C
      SAVE

C
C     Initial values
C
      DATA                  NCLASS / 0, 5, 6, 7, 8, 2 /


C
C     Begin every test family with an open call.
C
      CALL TOPEN  ( 'F_FRFTCH' )

C**********************************************************************
C
C
C     BLTFRM test cases
C
C
C**********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'BLTFRM test setup: get data for built-in frames.' )

C
C     Fetch built-in frame attributes.
C
      NCOUNT = NINERT + NNINRT

      CALL ZZFDAT ( NCOUNT, MAXBFR, NAMES,  IDCDES,
     .              CTRS,   TYPES,  TYPIDS, CENTRD,
     .              BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .              BIDLST, BIDPOL, BIDIDS, BIDIDX )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Initialize sets.
C
      CALL SSIZEI ( MAXBLT, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( MAXBLT, XIDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C---- Case -------------------------------------------------------------
C
C
C     Check the ability of BLTFRM to fetch all IDs of built-in
C     frames of a given class.
C
      DO CLSIDX = 1, 6

         TITLE = 'BLTFRM test: fetch class <class> built-in frames.'
         CALL REPMI ( TITLE, '<class>', CLSIDX, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C---- Case -------------------------------------------------------------
C
         CALL TCASE ( TITLE )

C
C        Empty the sets.
C
         CALL SCARDI ( 0, IDSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SCARDI ( 0, XIDSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Get the expected result set.
C
         N = 0

         DO I = 1, NCOUNT

            IF ( TYPES(I) .EQ. CLSIDX ) THEN

               CALL APPNDI ( IDCDES(I), XIDSET )
               CALL CHCKXC ( .FALSE., ' ', OK )

               N = N + 1

            END IF

         END DO

         CALL VALIDI ( MAXBLT, N, XIDSET )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Now fetch from BLTFRM the built-in frames of class CLSIDX.
C
         CALL BLTFRM ( CLSIDX, IDSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check set cardinality first.
C
         CALL CHCKSI ( 'Card(IDSET)', CARDI(IDSET), '=',
     .                                CARDI(XIDSET), 0,  OK )

         IF ( OK ) THEN
C
C           Check set contents.
C
            CALL CHCKAI ( 'IDSET', IDSET,  '=',
     .                             XIDSET, CARDI(IDSET), OK )
         END IF


      END DO


C
C---- Case -------------------------------------------------------------
C
C
C     Check the ability of BLTFRM to fetch all IDs of built-in
C     frames.
C
      TITLE = 'BLTFRM test: fetch IDs of all built-in frames.'

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( TITLE )

C
C     Empty the sets.
C
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCARDI ( 0, XIDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the expected result set.
C
      N = NCOUNT

      CALL MOVEI ( IDCDES, NCOUNT, XIDSET(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VALIDI ( MAXBLT, N, XIDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Now fetch the inertial, built-in frames from BLTFRM.
C
      CALL BLTFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check set cardinality first.
C
      CALL CHCKSI ( 'Card(IDSET)', CARDI(IDSET), '=',
     .                             CARDI(XIDSET), 0,  OK )

      IF ( OK ) THEN
C
C        Check set contents.
C
         CALL CHCKAI ( 'IDSET', IDSET,  '=',
     .                          XIDSET, CARDI(IDSET), OK )
      END IF

C
C
C     BLTFRM error cases
C
C

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'BLTFRM: bad frame class.' )

C
C     Class 0:
C
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BLTFRM ( 0, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAMECLASS)', OK )

C
C     Class -2:
C
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BLTFRM ( -2, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAMECLASS)', OK )

C
C     Class 7:
C
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BLTFRM ( 7, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAMECLASS)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'BLTFRM: output set too small.' )

C
C     Set the frame ID set size to 3.
C
      CALL SSIZEI ( 3, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Try to fetch the built-in inertial frames.
C
      CALL BLTFRM ( INERTL, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(SETTOOSMALL)', OK )



C**********************************************************************
C
C
C     KPLFRM test cases
C
C
C**********************************************************************


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'KPLFRM: setup: insert frame specs into pool.' )

C
C     Create a set of frame specifications and insert these into
C     the kernel pool. Frame classes 2-6 will be represented in
C     this set.
C

      DO CLSIDX = 2, 6

         DO I = 1, NCLASS(CLSIDX)

            FRNAME = 'FRCLASS_<class>_FRAME_<n>'
            CALL REPMI ( FRNAME, '<class>', CLSIDX, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( FRNAME, '<n>', I, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            KVNAME = 'FRAME_<name>'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Create an ID code for this frame.
C
            FRMID = ( 100000 * CLSIDX ) + I
C
C           Insert the frame ID assignment into the pool.
C
            CALL PIPOOL ( KVNAME, 1, FRMID )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Create a frame name assignment and insert this
C           into the pool.
C
            KVNAME = 'FRAME_<ID>_NAME'
            CALL REPMI ( KVNAME, '<ID>', FRMID, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL PCPOOL ( KVNAME, 1, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Insert the frame class into the pool.
C
            KVNAME = 'FRAME_<name>_CLASS'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCLSS = CLSIDX
            CALL PIPOOL ( KVNAME, 1, FRCLSS )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Insert the frame class ID into the pool. Make
C           the class ID the negative of the ID.
C
            KVNAME = 'FRAME_<name>_CLASS_ID'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCLID = - FRMID
            CALL PIPOOL ( KVNAME, 1, FRCLID )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Insert the frame center into the pool. Make
C           the center 10x the ID.
C
            KVNAME = 'FRAME_<name>_CENTER'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCENT = 10 * FRMID
            CALL PIPOOL ( KVNAME, 1, FRCENT )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

C
C---- Case -------------------------------------------------------------
C
C
C     Check the ability of KPLFRM to fetch all IDs of kernel pool
C     frames of a given class.
C
      DO CLSIDX = 1, 6

         TITLE = 'KPLFRM test: fetch class <class> kernel pool frames.'
         CALL REPMI ( TITLE, '<class>', CLSIDX, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C---- Case -------------------------------------------------------------
C
         CALL TCASE ( TITLE )

C
C        Initialize the sets.
C
         CALL SSIZEI ( MAXFRM, IDSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SSIZEI ( MAXFRM, XIDSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Get the expected result set.
C
         N = 0

         DO I = 1, NCLASS( CLSIDX )

            FRCODE =  ( 100000 * CLSIDX ) + I

            CALL APPNDI ( FRCODE, XIDSET )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = N + 1

         END DO

         CALL VALIDI ( MAXFRM, N, XIDSET )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Now fetch from KPLFRM the IDs of the kernel pool frames
C        of class CLSIDX.
C
         CALL KPLFRM ( CLSIDX, IDSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check set cardinality first.
C
         CALL CHCKSI ( 'Card(IDSET)', CARDI(IDSET), '=',
     .                                CARDI(XIDSET), 0,  OK )

         IF ( OK ) THEN
C
C           Check set contents.
C
            CALL CHCKAI ( 'IDSET', IDSET,  '=',
     .                             XIDSET, CARDI(IDSET), OK )
         END IF


      END DO



C
C---- Case -------------------------------------------------------------
C
C
C     Check the ability of KPLFRM to fetch all IDs of kernel pool
C     frames.
C
      CALL TCASE ( 'KPLFRM: fetch IDs of all loaded frames.' )

C
C     Empty the sets.
C
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCARDI ( 0, XIDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Form a set containing the IDs of all loaded frames.
C
      N = 0

      DO CLSIDX = 2, 6

         DO I = 1, NCLASS(CLSIDX)
C
C           Create an ID code for this frame.
C
            FRMID = ( 100000 * CLSIDX ) + I

            CALL APPNDI ( FRMID, XIDSET )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = N + 1

         END DO

      END DO

      CALL VALIDI ( MAXFRM, N, XIDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Now fetch from KPLFRM the IDs of the kernel pool frames.
C
      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check set cardinality first.
C
      CALL CHCKSI ( 'Card(IDSET)', CARDI(IDSET), '=',
     .                             CARDI(XIDSET), 0,  OK )

      IF ( OK ) THEN
C
C        Check set contents.
C
         CALL CHCKAI ( 'IDSET', IDSET,  '=',
     .                          XIDSET, CARDI(IDSET), OK )
      END IF


C
C     Non-error exceptions:
C

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'KPLFRM: make sure no error is signaled '
     .//           'on an attempt to fetch IDs of class 1 '
     .//           'frames. The returned set should be empty.'  )

      CALL SSIZEI ( MAXFRM, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KPLFRM ( INERTL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check set cardinality.
C
      CALL CHCKSI ( 'Card(IDSET)', CARDI(IDSET), '=',
     .                             0, 0,  OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'KPLFRM: make sure no error is signaled '
     .//           'and no frames are found if only FRAME_<name> '
     .//           'assignments are present.'                     )


      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Create a set of frame specifications and insert these into
C     the kernel pool. Frame classes 2-6 will be represented in
C     this set.
C

      DO CLSIDX = 2, 6

         DO I = 1, NCLASS(CLSIDX)

            FRNAME = 'FRCLASS_<class>_FRAME_<n>'
            CALL REPMI ( FRNAME, '<class>', CLSIDX, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( FRNAME, '<n>', I, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            KVNAME = 'FRAME_<name>'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Create an ID code for this frame.
C
            FRMID = ( 100000 * CLSIDX ) + I
C
C           Insert the frame ID assignment into the pool.
C
            CALL PIPOOL ( KVNAME, 1, FRMID )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

C
C     Fetch all of the frames. We shouldn't find any.
C
      CALL SSIZEI ( MAXFRM, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'CARDI(IDSET)', CARDI(IDSET), '=', 0, 0, OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'KPLFRM: make sure no error is signaled '
     .//           'and no frames are found if only FRAME_<name> '
     .//           'and FRAME_<ID code>_NAME assignments are present.' )


      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Create a set of frame specifications and insert these into
C     the kernel pool. Frame classes 2-6 will be represented in
C     this set.
C

      DO CLSIDX = 2, 6

         DO I = 1, NCLASS(CLSIDX)

            FRNAME = 'FRCLASS_<class>_FRAME_<n>'
            CALL REPMI ( FRNAME, '<class>', CLSIDX, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( FRNAME, '<n>', I, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            KVNAME = 'FRAME_<name>'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Create an ID code for this frame.
C
            FRMID = ( 100000 * CLSIDX ) + I
C
C           Insert the frame ID assignment into the pool.
C
            CALL PIPOOL ( KVNAME, 1, FRMID )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Create a frame name assignment and insert this
C           into the pool.
C
            KVNAME = 'FRAME_<ID>_NAME'
            CALL REPMI ( KVNAME, '<ID>', FRMID, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL PCPOOL ( KVNAME, 1, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

C
C     Fetch all of the frames. We shouldn't find any.
C
      CALL SSIZEI ( MAXFRM, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'CARDI(IDSET)', CARDI(IDSET), '=', 0, 0, OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'KPLFRM: make sure no error is signaled '
     .//           'and no frames are found if only FRAME_<name> '
     .//           'and FRAME_<name>_CLASS assignments are present.' )


      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Create a set of frame specifications and insert these into
C     the kernel pool. Frame classes 2-6 will be represented in
C     this set.
C

      DO CLSIDX = 2, 6

         DO I = 1, NCLASS(CLSIDX)

            FRNAME = 'FRCLASS_<class>_FRAME_<n>'
            CALL REPMI ( FRNAME, '<class>', CLSIDX, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( FRNAME, '<n>', I, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            KVNAME = 'FRAME_<name>'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Create an ID code for this frame.
C
            FRMID = ( 100000 * CLSIDX ) + I
C
C           Insert the frame ID assignment into the pool.
C
            CALL PIPOOL ( KVNAME, 1, FRMID )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Insert the frame class into the pool.
C
            KVNAME = 'FRAME_<name>_CLASS'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCLSS = CLSIDX
            CALL PIPOOL ( KVNAME, 1, FRCLSS )
            CALL CHCKXC ( .FALSE., ' ', OK )


         END DO

      END DO

C
C     Fetch all of the frames. We shouldn't find any.
C
      CALL SSIZEI ( MAXFRM, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'CARDI(IDSET)', CARDI(IDSET), '=', 0, 0, OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'KPLFRM: make sure no error is signaled '
     .//           'and no frames are found if only FRAME_<name> '
     .//           'and FRAME_<ID code>_CLASS assignments are present.')


      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Create a set of frame specifications and insert these into
C     the kernel pool. Frame classes 2-6 will be represented in
C     this set.
C

      DO CLSIDX = 2, 6

         DO I = 1, NCLASS(CLSIDX)

            FRNAME = 'FRCLASS_<class>_FRAME_<n>'
            CALL REPMI ( FRNAME, '<class>', CLSIDX, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( FRNAME, '<n>', I, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            KVNAME = 'FRAME_<name>'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Create an ID code for this frame.
C
            FRMID = ( 100000 * CLSIDX ) + I
C
C           Insert the frame ID assignment into the pool.
C
            CALL PIPOOL ( KVNAME, 1, FRMID )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Insert the frame class into the pool.
C
            KVNAME = 'FRAME_<code>_CLASS'
            CALL REPMI ( KVNAME, '<code>', FRMID, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCLSS = CLSIDX
            CALL PIPOOL ( KVNAME, 1, FRCLSS )
            CALL CHCKXC ( .FALSE., ' ', OK )


         END DO

      END DO

C
C     Fetch all of the frames. We shouldn't find any.
C
      CALL SSIZEI ( MAXFRM, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'CARDI(IDSET)', CARDI(IDSET), '=', 0, 0, OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'KPLFRM: make sure no error is signaled '
     .//           'and no frames are found if FRAME_<name> '
     .//           'and FRAME_<ID code>_NAME assignments are '
     .//           'present and the frame names don''t match.' )


      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Create a set of frame specifications and insert these into
C     the kernel pool. Frame classes 2-6 will be represented in
C     this set.
C

      DO CLSIDX = 2, 6

         DO I = 1, NCLASS(CLSIDX)

            FRNAME = 'FRCLASS_<class>_FRAME_<n>'
            CALL REPMI ( FRNAME, '<class>', CLSIDX, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( FRNAME, '<n>', I, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            KVNAME = 'FRAME_<name>'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Create an ID code for this frame.
C
            FRMID = ( 100000 * CLSIDX ) + I
C
C           Insert the frame ID assignment into the pool.
C
            CALL PIPOOL ( KVNAME, 1, FRMID )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Insert a non-matching frame name into the pool.
C
            KVNAME = 'FRAME_<code>_NAME'
            CALL REPMI ( KVNAME, '<code>', FRMID, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL PCPOOL ( KVNAME, 1, 'XXX' )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

C
C     Fetch all of the frames. We shouldn't find any.
C
      CALL SSIZEI ( MAXFRM, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'CARDI(IDSET)', CARDI(IDSET), '=', 0, 0, OK )


C
C---- Case -------------------------------------------------------------
C

      CALL TCASE ( 'Check frame buffer handling for large frame '
     .//           'counts.'                                      )

      CALL SSIZEI ( BUFSIZ, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Insert a large number of frame specs into the kernel pool.
C     The specs don't have to be complete---only the parts checked
C     by KPLFRM are needed.
C
      BIGN   = BUFSIZ

      DO I = 1, BIGN

         CLSIDX = MOD(I,3) + 1

         FRNAME = 'FRCLASS_<class>_FRAME_<n>'
         CALL REPMI ( FRNAME, '<class>', CLSIDX, FRNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL REPMI ( FRNAME, '<n>', I, FRNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         KVNAME = 'FRAME_<name>'
         CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Create an ID code for this frame.
C
         FRMID = ( 100000 * CLSIDX ) + I
C
C        Insert the frame ID assignment into the pool.
C
         CALL PIPOOL ( KVNAME, 1, FRMID )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Store the frame ID we just created.
C
         IDLIST(I) = FRMID

C
C        Insert the assignment of the frame name into
C        the kernel pool.
C
         KVNAME = 'FRAME_<code>_NAME'
         CALL REPMI ( KVNAME, '<code>', FRMID, KVNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PCPOOL ( KVNAME, 1, FRNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Insert the frame's class into the pool.
C
         KVNAME = 'FRAME_<code>_CLASS'
         CALL REPMI ( KVNAME, '<code>', FRMID, KVNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PIPOOL ( KVNAME, 1, CLSIDX )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Now fetch the frame names.
C
      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      I = CARDI( IDSET )

      CALL CHCKSI ( 'IDSET card', I, '=', BIGN, 0, OK )

C
C     Sort the stored ID codes.
C
      CALL SHELLI ( BIGN, IDLIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure that the stored ID set matches the
C     set returned by KPLFRM.
C
      CALL CHCKAI ( 'IDSET', IDSET(1), '=', IDLIST, BIGN, OK )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Setup: restore frame specs.' )

C
C     Restore the frame specifications for use in later test cases.
C
      DO CLSIDX = 2, 6

         DO I = 1, NCLASS(CLSIDX)

            FRNAME = 'FRCLASS_<class>_FRAME_<n>'
            CALL REPMI ( FRNAME, '<class>', CLSIDX, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( FRNAME, '<n>', I, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            KVNAME = 'FRAME_<name>'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Create an ID code for this frame.
C
            FRMID = ( 100000 * CLSIDX ) + I
C
C           Insert the frame ID assignment into the pool.
C
            CALL PIPOOL ( KVNAME, 1, FRMID )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Create a frame name assignment and insert this
C           into the pool.
C
            KVNAME = 'FRAME_<ID>_NAME'
            CALL REPMI ( KVNAME, '<ID>', FRMID, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL PCPOOL ( KVNAME, 1, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Insert the frame class into the pool.
C
            KVNAME = 'FRAME_<name>_CLASS'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCLSS = CLSIDX
            CALL PIPOOL ( KVNAME, 1, FRCLSS )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Insert the frame class ID into the pool. Make
C           the class ID the negative of the ID.
C
            KVNAME = 'FRAME_<name>_CLASS_ID'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCLID = - FRMID
            CALL PIPOOL ( KVNAME, 1, FRCLID )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Insert the frame center into the pool. Make
C           the center 10x the ID.
C
            KVNAME = 'FRAME_<name>_CENTER'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCENT = 10 * FRMID
            CALL PIPOOL ( KVNAME, 1, FRCENT )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO


C
C     KPLFRM error cases
C
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'KPLFRM: bad frame class.' )

C
C     Class 0:
C
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KPLFRM ( 0, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAMECLASS)', OK )

C
C     Class -2:
C
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KPLFRM ( -2, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAMECLASS)', OK )

C
C     Class 7:
C
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KPLFRM ( 7, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAMECLASS)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'KPLFRM: output set too small.' )

C
C     Set the frame ID set size to 3.
C
      CALL SSIZEI ( 3, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Try to fetch all kernel pool frames.
C
      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(SETTOOSMALL)', OK )




      CALL T_SUCCESS( OK )
      RETURN
      END
