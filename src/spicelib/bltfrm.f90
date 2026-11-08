!$Procedure BLTFRM ( Built-in frame IDs )

      SUBROUTINE BLTFRM ( FRMCLS, IDSET )

! $ Abstract
!
!     Return a set containing the frame IDs of all built-in frames of a
!     specified class.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     CELLS
!     FRAMES
!     NAIF_IDS
!     SETS
!
! $ Keywords
!
!     FRAME
!     SET
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'frmtyp.inc'
      INCLUDE 'ninert.inc'
      INCLUDE 'nninrt.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               FRMCLS
      INTEGER               IDSET  ( LBCELL : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FRMCLS     I   Frame class.
!     IDSET      O   Set of ID codes of frames of the specified class.
!
! $ Detailed_Input
!
!     FRMCLS   is an integer code specifying the frame class or
!              classes for which built-in frame ID codes are
!              requested. FRMCLS may designate a single class or
!              "all classes."
!
!              The include file frmtyp.inc declares parameters
!              identifying frame classes. The supported values
!              and corresponding meanings of FRMCLS are
!
!                 Parameter      Value    Meaning
!                 =========      =====    =================
!                 ALL              -1     All frame classes
!                 INERTL            1     Built-in inertial
!                 PCK               2     PCK-based frame
!                 CK                3     CK-based frame
!                 TK                4     Fixed offset ("text
!                                         kernel") frame
!                 DYN               5     Dynamic frame
!                 SWTCH             6     Switch frame
!
! $ Detailed_Output
!
!     IDSET    is a SPICE set containing the ID codes of all
!              built-in reference frames of the specified class
!              or classes.
!
!              If IDSET is non-empty on input, its contents will be
!              discarded.
!
!              IDSET must be initialized by the caller via the
!              SPICELIB routine SSIZEI.
!
! $ Parameters
!
!     See the INCLUDE file frmtyp.inc.
!
! $ Exceptions
!
!     1)  If the input frame class argument is not defined in
!         frmtyp.inc, the error SPICE(BADFRAMECLASS) is signaled.
!
!     2)  If the size of IDSET is too small to hold the requested
!         frame ID set, the error SPICE(SETTOOSMALL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine has a counterpart
!
!        KPLFRM
!
!     which fetches the frame IDs of all frames specified in the kernel
!     pool.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Display the IDs and names of all SPICE built-in frames.
!        Group the outputs by frame class. Also fetch and display
!        the entire set of IDs and names using the parameter ALL.
!
!        Example code begins here.
!
!
!              PROGRAM BLTFRM_EX1
!              IMPLICIT NONE
!
!              INCLUDE 'ninert.inc'
!              INCLUDE 'nninrt.inc'
!              INCLUDE 'frmtyp.inc'
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               CARDI
!        C
!        C     Local parameters
!        C
!              INTEGER               NFRAME
!              PARAMETER           ( NFRAME = NINERT + NNINRT )
!
!              INTEGER               LBCELL
!              PARAMETER           ( LBCELL = -5 )
!
!              INTEGER               LNSIZE
!              PARAMETER           ( LNSIZE = 80 )
!
!              INTEGER               FRNMLN
!              PARAMETER           ( FRNMLN = 32 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(FRNMLN)    FRNAME
!              CHARACTER*(LNSIZE)    OUTLIN
!              CHARACTER*(LNSIZE)    VERSN
!
!              INTEGER               I
!              INTEGER               IDSET ( LBCELL : NFRAME )
!              INTEGER               NFRMS
!              INTEGER               J
!
!        C
!        C     Get the Toolkit version number and display it.
!        C
!              CALL TKVRSN ( 'TOOLKIT', VERSN )
!              CALL TOSTDO ( 'Toolkit version: ' // VERSN )
!
!        C
!        C     Initialize the frame set.
!        C
!              CALL SSIZEI ( NFRAME, IDSET )
!
!        C
!        C     Fetch and display the frames of each class.
!        C
!              DO I = 1, 7
!
!                 IF ( I .LT. 7 ) THEN
!        C
!        C           Fetch the frames of class I.
!        C
!                    CALL BLTFRM ( I, IDSET )
!
!                    OUTLIN = 'Number of frames of class #: #'
!                    CALL REPMI ( OUTLIN, '#', I,            OUTLIN )
!                    CALL REPMI ( OUTLIN, '#', CARDI(IDSET), OUTLIN )
!
!                 ELSE
!        C
!        C           Fetch IDs of all built-in frames.
!        C
!                    CALL BLTFRM ( ALL, IDSET )
!
!                    OUTLIN = 'Number of built-in frames: #'
!                    CALL REPMI ( OUTLIN, '#', CARDI(IDSET), OUTLIN )
!
!                 END IF
!
!                 CALL TOSTDO ( ' '    )
!                 CALL TOSTDO ( OUTLIN )
!                 CALL TOSTDO ( '   Frame IDs and names' )
!
!        C
!        C        Display the NAIF ID and name of a maximum of 5 frames
!        C        per family.
!        C
!                 NFRMS = MIN( 5, CARDI(IDSET) )
!
!                 DO J = 1, NFRMS
!                    CALL FRMNAM ( IDSET(J), FRNAME )
!                    WRITE (*,*) IDSET(J), '  ', FRNAME
!                 END DO
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Toolkit version: N0067
!
!        Number of frames of class 1: 21
!           Frame IDs and names
!                   1   J2000
!                   2   B1950
!                   3   FK4
!                   4   DE-118
!                   5   DE-96
!
!        Number of frames of class 2: 105
!           Frame IDs and names
!               10001   IAU_MERCURY_BARYCENTER
!               10002   IAU_VENUS_BARYCENTER
!               10003   IAU_EARTH_BARYCENTER
!               10004   IAU_MARS_BARYCENTER
!               10005   IAU_JUPITER_BARYCENTER
!
!        Number of frames of class 3: 0
!           Frame IDs and names
!
!        Number of frames of class 4: 1
!           Frame IDs and names
!               10081   EARTH_FIXED
!
!        Number of frames of class 5: 0
!           Frame IDs and names
!
!        Number of frames of class 6: 0
!           Frame IDs and names
!
!        Number of built-in frames: 127
!           Frame IDs and names
!                   1   J2000
!                   2   B1950
!                   3   FK4
!                   4   DE-118
!                   5   DE-96
!
!
!        Note that the set of built-in frames, particularly the
!        non-inertial ones, will grow over time, so the output
!        shown here may be out of sync with that produced by a
!        current SPICE Toolkit. Only the first 5 frames of each
!        family are presented in the output.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 08-AUG-2021 (JDR) (NJB)
!
!        Updated to account for switch frame class.
!
!        Edited the header to comply with NAIF standard. Updated
!        code example to limit the number of frames presented in the
!        output.
!
!        Extended IDSET description to indicate that the set must
!        be declared and initialized before calling this routine and
!        that its contents will be discarded.
!
!-    SPICELIB Version 1.1.0, 09-AUG-2013 (BVS)
!
!        Updated for changed ZZFDAT calling sequence.
!
!-    SPICELIB Version 1.0.0, 21-MAY-2012 (NJB)
!
!-&


! $ Index_Entries
!
!     fetch IDs of built-in reference frames
!
!-&


!
!     SPICELIB functions
!
      INTEGER               SIZEI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               NFRAME
      PARAMETER           ( NFRAME = NINERT + NNINRT )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

!
!     Local variables
!
      CHARACTER*(FRNMLN)    FRNAME ( NFRAME )

      INTEGER               CTRORD ( NFRAME )
      INTEGER               CENTER ( NFRAME )
      INTEGER               CORDER ( NFRAME )
      INTEGER               FCLASS ( NFRAME )
      INTEGER               FCLSID ( NFRAME )
      INTEGER               FCODE  ( NFRAME )
      INTEGER               I
      INTEGER               J
      INTEGER               TO

      LOGICAL               PASS1

!
!     Built-in frame hashes returned by ZZFDAT.
!
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               MAXBFR
      PARAMETER           ( MAXBFR = NFRAME+1 )

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
!
!     Save all variables in order to avoid problems
!     in code translated by f2c.
!
      SAVE

!
!     Initial values
!
      DATA                  PASS1 / .TRUE. /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'BLTFRM' )
!
!     The output set starts out empty.
!
      CALL SCARDI ( 0, IDSET )

!
!     On the first pass, fetch all data for the
!     built-in frames.
!
      IF ( PASS1 ) THEN

         CALL ZZFDAT ( NFRAME, MAXBFR, FRNAME, FCODE, &
                       CENTER, FCLASS, FCLSID, CTRORD, &
                       BNMLST, BNMPOL, BNMNMS, BNMIDX, &
                       BIDLST, BIDPOL, BIDIDS, BIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'BLTFRM' )
            RETURN
         END IF

         PASS1 = .FALSE.

      END IF

!
!     Check the input frame class.
!
!     This block of code must be kept in sync with frmtyp.inc.
!
      IF  (      ( FRMCLS .GT. SWTCH ) &
            .OR. ( FRMCLS .EQ. 0   ) &
            .OR. ( FRMCLS .LT. ALL )  ) THEN

         CALL SETMSG ( 'Frame class specifier FRMCLS was #; this ' &
         //            'value is not supported.'                  )
         CALL ERRINT ( '#',  FRMCLS                               )
         CALL SIGERR ( 'SPICE(BADFRAMECLASS)'                     )
         CALL CHKOUT ( 'BLTFRM'                                   )
         RETURN

      END IF

!
!     Make sure the set is large enough to hold all of
!     the IDs of the built-in frames.
!
      IF ( SIZEI(IDSET) .LT. NFRAME ) THEN

         CALL SETMSG ( 'Frame ID set argument IDSET has size #; ' &
         //            'required size is at least #.'           )
         CALL ERRINT ( '#', SIZEI(IDSET)                        )
         CALL ERRINT ( '#', NFRAME                              )
         CALL SIGERR ( 'SPICE(SETTOOSMALL)'                     )
         CALL CHKOUT ( 'BLTFRM'                                 )
         RETURN

      END IF

!
!     Transfer ID codes of all frames of the specified class
!     to the output set. First, generate an order vector for
!     the ID codes.
!
      CALL ORDERI ( FCODE, NFRAME, CORDER )

      TO = 0

      DO I = 1, NFRAME
!
!        Get the index J in the parallel data arrays of
!        the Ith frame, ordered by ID code.
!
         J = CORDER(I)

         IF (      ( FCLASS(J) .EQ. FRMCLS ) &
              .OR. ( FRMCLS    .EQ. ALL    )  ) THEN
!
!           The frame at index J belongs to the
!           requested class. Append the frame's ID
!           code to the set.
!
            TO        = TO + 1
            IDSET(TO) = FCODE(J)

         END IF

      END DO

!
!     Set the cardinality of the output set.
!
!     Note that we populated the set using an order vector, so sorting
!     the elements is not necessary. We rely on ZZFDAT to not give us
!     duplicate frame specifications.
!
      CALL SCARDI ( TO, IDSET )

      CALL CHKOUT ( 'BLTFRM' )
      RETURN
      END
