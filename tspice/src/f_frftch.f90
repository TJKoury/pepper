!$Procedure      F_FRFTCH ( Family of tests for frame fetch APIs )

      SUBROUTINE F_FRFTCH ( OK )
      use pepper_inc_frmtyp
      use pepper_inc_ninert
      use pepper_inc_nninrt

! $ Abstract
!
!     This test family checks out the frame ID fetch APIs.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE


      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting. The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the frame ID code fetch APIs listed below:
!
!        BLTFRM
!        KPLFRM
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 3.0.0, 14-OCT-2020 (NJB)
!
!        Updated to account for switch frame class.
!
!-    TSPICE Version 2.0.0, 09-FEB-2016 (NJB)
!
!-       22-JUN-2015 (NJB)
!
!        Updated to exercise KPLFRM buffering logic.
!
!-    TSPICE Version 1.1.0, 09-AUG-2013 (BVS)
!
!        Updated for changed ZZFDAT calling sequence.
!
!-    TSPICE Version 1.0.0, 23-MAY-2012 (NJB)
!
!-&



!
!     Test Utility Functions
!

!
!     SPICELIB Functions
!
      INTEGER               CARDI

!
!     Local parameters
!
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

!
!     Local Variables
!
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

!
!     Built-in frame hashes returned by ZZFDAT.
!
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

!
!     Saved variables
!
!     Save all variables to avoid CSPICE stack overflow
!     problems.
!
      SAVE

!
!     Initial values
!
      DATA                  NCLASS / 0, 5, 6, 7, 8, 2 /


!
!     Begin every test family with an open call.
!
      CALL TOPEN  ( 'F_FRFTCH' )

!**********************************************************************
!
!
!     BLTFRM test cases
!
!
!**********************************************************************

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'BLTFRM test setup: get data for built-in frames.' )

!
!     Fetch built-in frame attributes.
!
      NCOUNT = NINERT + NNINRT

      CALL ZZFDAT ( NCOUNT, MAXBFR, NAMES,  IDCDES, &
                    CTRS,   TYPES,  TYPIDS, CENTRD, &
                    BNMLST, BNMPOL, BNMNMS, BNMIDX, &
                    BIDLST, BIDPOL, BIDIDS, BIDIDX )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Initialize sets.
!
      CALL SSIZEI ( MAXBLT, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( MAXBLT, XIDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!---- Case -------------------------------------------------------------
!
!
!     Check the ability of BLTFRM to fetch all IDs of built-in
!     frames of a given class.
!
      DO CLSIDX = 1, 6

         TITLE = 'BLTFRM test: fetch class <class> built-in frames.'
         CALL REPMI ( TITLE, '<class>', CLSIDX, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!---- Case -------------------------------------------------------------
!
         CALL TCASE ( TITLE )

!
!        Empty the sets.
!
         CALL SCARDI ( 0, IDSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SCARDI ( 0, XIDSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Get the expected result set.
!
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


!
!        Now fetch from BLTFRM the built-in frames of class CLSIDX.
!
         CALL BLTFRM ( CLSIDX, IDSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check set cardinality first.
!
         CALL CHCKSI ( 'Card(IDSET)', CARDI(IDSET), '=', &
                                      CARDI(XIDSET), 0,  OK )

         IF ( OK ) THEN
!
!           Check set contents.
!
            CALL CHCKAI ( 'IDSET', IDSET,  '=', &
                                   XIDSET, CARDI(IDSET), OK )
         END IF


      END DO


!
!---- Case -------------------------------------------------------------
!
!
!     Check the ability of BLTFRM to fetch all IDs of built-in
!     frames.
!
      TITLE = 'BLTFRM test: fetch IDs of all built-in frames.'

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( TITLE )

!
!     Empty the sets.
!
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCARDI ( 0, XIDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the expected result set.
!
      N = NCOUNT

      CALL MOVEI ( IDCDES, NCOUNT, XIDSET(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VALIDI ( MAXBLT, N, XIDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now fetch the inertial, built-in frames from BLTFRM.
!
      CALL BLTFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check set cardinality first.
!
      CALL CHCKSI ( 'Card(IDSET)', CARDI(IDSET), '=', &
                                   CARDI(XIDSET), 0,  OK )

      IF ( OK ) THEN
!
!        Check set contents.
!
         CALL CHCKAI ( 'IDSET', IDSET,  '=', &
                                XIDSET, CARDI(IDSET), OK )
      END IF

!
!
!     BLTFRM error cases
!
!

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'BLTFRM: bad frame class.' )

!
!     Class 0:
!
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BLTFRM ( 0, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAMECLASS)', OK )

!
!     Class -2:
!
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BLTFRM ( -2, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAMECLASS)', OK )

!
!     Class 7:
!
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BLTFRM ( 7, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAMECLASS)', OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'BLTFRM: output set too small.' )

!
!     Set the frame ID set size to 3.
!
      CALL SSIZEI ( 3, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Try to fetch the built-in inertial frames.
!
      CALL BLTFRM ( INERTL, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(SETTOOSMALL)', OK )



!**********************************************************************
!
!
!     KPLFRM test cases
!
!
!**********************************************************************


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'KPLFRM: setup: insert frame specs into pool.' )

!
!     Create a set of frame specifications and insert these into
!     the kernel pool. Frame classes 2-6 will be represented in
!     this set.
!

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
!
!           Create an ID code for this frame.
!
            FRMID = ( 100000 * CLSIDX ) + I
!
!           Insert the frame ID assignment into the pool.
!
            CALL PIPOOL ( KVNAME, 1, FRMID )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Create a frame name assignment and insert this
!           into the pool.
!
            KVNAME = 'FRAME_<ID>_NAME'
            CALL REPMI ( KVNAME, '<ID>', FRMID, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL PCPOOL ( KVNAME, 1, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Insert the frame class into the pool.
!
            KVNAME = 'FRAME_<name>_CLASS'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCLSS = CLSIDX
            CALL PIPOOL ( KVNAME, 1, FRCLSS )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Insert the frame class ID into the pool. Make
!           the class ID the negative of the ID.
!
            KVNAME = 'FRAME_<name>_CLASS_ID'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCLID = - FRMID
            CALL PIPOOL ( KVNAME, 1, FRCLID )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Insert the frame center into the pool. Make
!           the center 10x the ID.
!
            KVNAME = 'FRAME_<name>_CENTER'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCENT = 10 * FRMID
            CALL PIPOOL ( KVNAME, 1, FRCENT )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

!
!---- Case -------------------------------------------------------------
!
!
!     Check the ability of KPLFRM to fetch all IDs of kernel pool
!     frames of a given class.
!
      DO CLSIDX = 1, 6

         TITLE = 'KPLFRM test: fetch class <class> kernel pool frames.'
         CALL REPMI ( TITLE, '<class>', CLSIDX, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!---- Case -------------------------------------------------------------
!
         CALL TCASE ( TITLE )

!
!        Initialize the sets.
!
         CALL SSIZEI ( MAXFRM, IDSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SSIZEI ( MAXFRM, XIDSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Get the expected result set.
!
         N = 0

         DO I = 1, NCLASS( CLSIDX )

            FRCODE =  ( 100000 * CLSIDX ) + I

            CALL APPNDI ( FRCODE, XIDSET )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = N + 1

         END DO

         CALL VALIDI ( MAXFRM, N, XIDSET )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Now fetch from KPLFRM the IDs of the kernel pool frames
!        of class CLSIDX.
!
         CALL KPLFRM ( CLSIDX, IDSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check set cardinality first.
!
         CALL CHCKSI ( 'Card(IDSET)', CARDI(IDSET), '=', &
                                      CARDI(XIDSET), 0,  OK )

         IF ( OK ) THEN
!
!           Check set contents.
!
            CALL CHCKAI ( 'IDSET', IDSET,  '=', &
                                   XIDSET, CARDI(IDSET), OK )
         END IF


      END DO



!
!---- Case -------------------------------------------------------------
!
!
!     Check the ability of KPLFRM to fetch all IDs of kernel pool
!     frames.
!
      CALL TCASE ( 'KPLFRM: fetch IDs of all loaded frames.' )

!
!     Empty the sets.
!
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SCARDI ( 0, XIDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Form a set containing the IDs of all loaded frames.
!
      N = 0

      DO CLSIDX = 2, 6

         DO I = 1, NCLASS(CLSIDX)
!
!           Create an ID code for this frame.
!
            FRMID = ( 100000 * CLSIDX ) + I

            CALL APPNDI ( FRMID, XIDSET )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = N + 1

         END DO

      END DO

      CALL VALIDI ( MAXFRM, N, XIDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now fetch from KPLFRM the IDs of the kernel pool frames.
!
      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check set cardinality first.
!
      CALL CHCKSI ( 'Card(IDSET)', CARDI(IDSET), '=', &
                                   CARDI(XIDSET), 0,  OK )

      IF ( OK ) THEN
!
!        Check set contents.
!
         CALL CHCKAI ( 'IDSET', IDSET,  '=', &
                                XIDSET, CARDI(IDSET), OK )
      END IF


!
!     Non-error exceptions:
!

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'KPLFRM: make sure no error is signaled ' &
      //           'on an attempt to fetch IDs of class 1 ' &
      //           'frames. The returned set should be empty.'  )

      CALL SSIZEI ( MAXFRM, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KPLFRM ( INERTL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check set cardinality.
!
      CALL CHCKSI ( 'Card(IDSET)', CARDI(IDSET), '=', &
                                   0, 0,  OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'KPLFRM: make sure no error is signaled ' &
      //           'and no frames are found if only FRAME_<name> ' &
      //           'assignments are present.'                     )


      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Create a set of frame specifications and insert these into
!     the kernel pool. Frame classes 2-6 will be represented in
!     this set.
!

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
!
!           Create an ID code for this frame.
!
            FRMID = ( 100000 * CLSIDX ) + I
!
!           Insert the frame ID assignment into the pool.
!
            CALL PIPOOL ( KVNAME, 1, FRMID )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

!
!     Fetch all of the frames. We shouldn't find any.
!
      CALL SSIZEI ( MAXFRM, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'CARDI(IDSET)', CARDI(IDSET), '=', 0, 0, OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'KPLFRM: make sure no error is signaled ' &
      //           'and no frames are found if only FRAME_<name> ' &
      //           'and FRAME_<ID code>_NAME assignments are present.' )


      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Create a set of frame specifications and insert these into
!     the kernel pool. Frame classes 2-6 will be represented in
!     this set.
!

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
!
!           Create an ID code for this frame.
!
            FRMID = ( 100000 * CLSIDX ) + I
!
!           Insert the frame ID assignment into the pool.
!
            CALL PIPOOL ( KVNAME, 1, FRMID )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Create a frame name assignment and insert this
!           into the pool.
!
            KVNAME = 'FRAME_<ID>_NAME'
            CALL REPMI ( KVNAME, '<ID>', FRMID, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL PCPOOL ( KVNAME, 1, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

!
!     Fetch all of the frames. We shouldn't find any.
!
      CALL SSIZEI ( MAXFRM, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'CARDI(IDSET)', CARDI(IDSET), '=', 0, 0, OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'KPLFRM: make sure no error is signaled ' &
      //           'and no frames are found if only FRAME_<name> ' &
      //           'and FRAME_<name>_CLASS assignments are present.' )


      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Create a set of frame specifications and insert these into
!     the kernel pool. Frame classes 2-6 will be represented in
!     this set.
!

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
!
!           Create an ID code for this frame.
!
            FRMID = ( 100000 * CLSIDX ) + I
!
!           Insert the frame ID assignment into the pool.
!
            CALL PIPOOL ( KVNAME, 1, FRMID )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Insert the frame class into the pool.
!
            KVNAME = 'FRAME_<name>_CLASS'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCLSS = CLSIDX
            CALL PIPOOL ( KVNAME, 1, FRCLSS )
            CALL CHCKXC ( .FALSE., ' ', OK )


         END DO

      END DO

!
!     Fetch all of the frames. We shouldn't find any.
!
      CALL SSIZEI ( MAXFRM, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'CARDI(IDSET)', CARDI(IDSET), '=', 0, 0, OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'KPLFRM: make sure no error is signaled ' &
      //           'and no frames are found if only FRAME_<name> ' &
      //           'and FRAME_<ID code>_CLASS assignments are present.')


      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Create a set of frame specifications and insert these into
!     the kernel pool. Frame classes 2-6 will be represented in
!     this set.
!

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
!
!           Create an ID code for this frame.
!
            FRMID = ( 100000 * CLSIDX ) + I
!
!           Insert the frame ID assignment into the pool.
!
            CALL PIPOOL ( KVNAME, 1, FRMID )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Insert the frame class into the pool.
!
            KVNAME = 'FRAME_<code>_CLASS'
            CALL REPMI ( KVNAME, '<code>', FRMID, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCLSS = CLSIDX
            CALL PIPOOL ( KVNAME, 1, FRCLSS )
            CALL CHCKXC ( .FALSE., ' ', OK )


         END DO

      END DO

!
!     Fetch all of the frames. We shouldn't find any.
!
      CALL SSIZEI ( MAXFRM, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'CARDI(IDSET)', CARDI(IDSET), '=', 0, 0, OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'KPLFRM: make sure no error is signaled ' &
      //           'and no frames are found if FRAME_<name> ' &
      //           'and FRAME_<ID code>_NAME assignments are ' &
      //           'present and the frame names don''t match.' )


      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Create a set of frame specifications and insert these into
!     the kernel pool. Frame classes 2-6 will be represented in
!     this set.
!

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
!
!           Create an ID code for this frame.
!
            FRMID = ( 100000 * CLSIDX ) + I
!
!           Insert the frame ID assignment into the pool.
!
            CALL PIPOOL ( KVNAME, 1, FRMID )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Insert a non-matching frame name into the pool.
!
            KVNAME = 'FRAME_<code>_NAME'
            CALL REPMI ( KVNAME, '<code>', FRMID, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL PCPOOL ( KVNAME, 1, 'XXX' )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO

!
!     Fetch all of the frames. We shouldn't find any.
!
      CALL SSIZEI ( MAXFRM, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'CARDI(IDSET)', CARDI(IDSET), '=', 0, 0, OK )


!
!---- Case -------------------------------------------------------------
!

      CALL TCASE ( 'Check frame buffer handling for large frame ' &
      //           'counts.'                                      )

      CALL SSIZEI ( BUFSIZ, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Insert a large number of frame specs into the kernel pool.
!     The specs don't have to be complete---only the parts checked
!     by KPLFRM are needed.
!
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

!
!        Create an ID code for this frame.
!
         FRMID = ( 100000 * CLSIDX ) + I
!
!        Insert the frame ID assignment into the pool.
!
         CALL PIPOOL ( KVNAME, 1, FRMID )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Store the frame ID we just created.
!
         IDLIST(I) = FRMID

!
!        Insert the assignment of the frame name into
!        the kernel pool.
!
         KVNAME = 'FRAME_<code>_NAME'
         CALL REPMI ( KVNAME, '<code>', FRMID, KVNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PCPOOL ( KVNAME, 1, FRNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Insert the frame's class into the pool.
!
         KVNAME = 'FRAME_<code>_CLASS'
         CALL REPMI ( KVNAME, '<code>', FRMID, KVNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PIPOOL ( KVNAME, 1, CLSIDX )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Now fetch the frame names.
!
      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      I = CARDI( IDSET )

      CALL CHCKSI ( 'IDSET card', I, '=', BIGN, 0, OK )

!
!     Sort the stored ID codes.
!
      CALL SHELLI ( BIGN, IDLIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure that the stored ID set matches the
!     set returned by KPLFRM.
!
      CALL CHCKAI ( 'IDSET', IDSET(1), '=', IDLIST, BIGN, OK )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Setup: restore frame specs.' )

!
!     Restore the frame specifications for use in later test cases.
!
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
!
!           Create an ID code for this frame.
!
            FRMID = ( 100000 * CLSIDX ) + I
!
!           Insert the frame ID assignment into the pool.
!
            CALL PIPOOL ( KVNAME, 1, FRMID )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Create a frame name assignment and insert this
!           into the pool.
!
            KVNAME = 'FRAME_<ID>_NAME'
            CALL REPMI ( KVNAME, '<ID>', FRMID, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL PCPOOL ( KVNAME, 1, FRNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Insert the frame class into the pool.
!
            KVNAME = 'FRAME_<name>_CLASS'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCLSS = CLSIDX
            CALL PIPOOL ( KVNAME, 1, FRCLSS )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Insert the frame class ID into the pool. Make
!           the class ID the negative of the ID.
!
            KVNAME = 'FRAME_<name>_CLASS_ID'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCLID = - FRMID
            CALL PIPOOL ( KVNAME, 1, FRCLID )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Insert the frame center into the pool. Make
!           the center 10x the ID.
!
            KVNAME = 'FRAME_<name>_CENTER'
            CALL REPMC ( KVNAME, '<name>', FRNAME, KVNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            FRCENT = 10 * FRMID
            CALL PIPOOL ( KVNAME, 1, FRCENT )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO


!
!     KPLFRM error cases
!
!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'KPLFRM: bad frame class.' )

!
!     Class 0:
!
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KPLFRM ( 0, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAMECLASS)', OK )

!
!     Class -2:
!
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KPLFRM ( -2, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAMECLASS)', OK )

!
!     Class 7:
!
      CALL SCARDI ( 0, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KPLFRM ( 7, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAMECLASS)', OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'KPLFRM: output set too small.' )

!
!     Set the frame ID set size to 3.
!
      CALL SSIZEI ( 3, IDSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Try to fetch all kernel pool frames.
!
      CALL KPLFRM ( ALL, IDSET )
      CALL CHCKXC ( .TRUE., 'SPICE(SETTOOSMALL)', OK )




      CALL T_SUCCESS( OK )
      RETURN
      END
