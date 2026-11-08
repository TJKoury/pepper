!$Procedure SGFREF ( Generic Segments: Fetch references )

      SUBROUTINE SGFREF ( HANDLE, DESCR, FIRST, LAST, VALUES )

! $ Abstract
!
!     Fetch from the references partition of a generic segment
!     the double precision numbers from FIRST to LAST. The
!     segment is identified by a DAF file handle and segment
!     descriptor.
!
! $ Required_Reading
!
!     DAF
!
! $ Keywords
!
!     GENERIC SEGMENTS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR  ( * )
      INTEGER               FIRST
      INTEGER               LAST
      DOUBLE PRECISION      VALUES ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a DAF open for reading.
!     DESCR      I   Descriptor for a generic segment in the DAF.
!     FIRST      I   The index of the first reference value to fetch.
!     LAST       I   The index of the last reference value to fetch.
!     VALUES     O   The reference values that were requested.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAF file opened for reading
!              that contains the segment described by DESCR.
!
!     DESCR    is the descriptor of the segment with the desired
!              constant values. This must be the descriptor for a
!              segment in the DAF associated with HANDLE.
!
!     FIRST    is the index of the first value to fetch from the
!              reference section of the DAF generic segment associated
!              with HANDLE and DESCR.
!
!     LAST     is the index of the last value to fetch from the
!              constants section of the DAF generic segment associated
!              with HANDLE and DESCR.
!
! $ Detailed_Output
!
!     VALUES   is the array of reference values obtained from the
!              reference section of the DAF generic segment
!              associated with HANDLE and DESCR.
!
! $ Parameters
!
!     This subroutine makes use of parameters defined in the file
!     'sgparam.inc'.
!
! $ Exceptions
!
!     1)  If FIRST is less than 1 or LAST is greater than the number of
!         reference values, the error SPICE(REQUESTOUTOFBOUNDS) is
!         signaled.
!
!     2)  If LAST is less than FIRST, the error SPICE(REQUESTOUTOFORDER)
!         is signaled.
!
!     3)  If the reference directory structure is unrecognized, the
!         error SPICE(UNKNOWNREFDIR) is signaled. The most likely cause
!         of this error is that an upgrade to your version of the SPICE
!         toolkit is needed.
!
! $ Files
!
!     See the description of HANDLE above.
!
! $ Particulars
!
!     This routine allows you to easily fetch values from the reference
!     section of a generic segment.
!
! $ Examples
!
!     Suppose that you have located a DAF generic segment. The code
!     fragment below shows how to fetch the I'th reference value from
!     that segment.
!
!        Declarations:
!
!        DOUBLE PRECISION      REFVAL
!
!        Fetch the Ith reference value from the segment.
!
!        CALL SGFREF ( HANDLE, DESCR, I, I, REFVAL )
!
! $ Restrictions
!
!     1)  The segment described by DESCR MUST be a generic segment,
!         otherwise the results of this routine are not predictable.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!
!-    SPICELIB Version 1.0.0, 12-APR-1995 (KRG) (WLT)
!
!-&


! $ Index_Entries
!
!     fetch reference values from a generic segment
!
!-&


!
!     SPICELIB Functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local Parameters
!
!     Include the mnemonic values for the generic segment declarations.
!
      INCLUDE 'sgparam.inc'

!
!     Local Variables
!
      DOUBLE PRECISION      BUFFER ( 2 )

      INTEGER               B
      INTEGER               BASE
      INTEGER               E
      INTEGER               I
      INTEGER               MYNREF
      INTEGER               MYREFT

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SGFREF' )
!
!     Perform the needed initialization
!
      CALL SGMETA ( HANDLE, DESCR, REFBAS, BASE   )
      CALL SGMETA ( HANDLE, DESCR, RDRTYP, MYREFT )
      CALL SGMETA ( HANDLE, DESCR, NREF,   MYNREF )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SGFREF' )
         RETURN
      END IF

!
!     Perform checks on the inputs for reasonableness.
!
      IF ( ( FIRST .LT. 1 ) .OR. ( LAST .GT. MYNREF) ) THEN

         CALL SETMSG ( 'The range of reference items requested' &
         //            ' extends beyond the available range of' &
         //            ' reference items.  The reference data is' &
         //            ' available' &
         //            ' for indexes 1 to #.  You''ve requested' &
         //            ' data from # to #.'                        )
         CALL ERRINT ( '#', MYNREF                                 )
         CALL ERRINT ( '#', FIRST                                  )
         CALL ERRINT ( '#', LAST                                   )
         CALL SIGERR ( 'SPICE(REQUESTOUTOFBOUNDS)'                 )
         CALL CHKOUT ( 'SGFREF'                                    )
         RETURN

      END IF

      IF ( LAST .LT. FIRST ) THEN

         CALL SETMSG ( 'The last reference item requested, #, is' &
         //            ' before the first reference item' &
         //            ' requested, #.'                            )
         CALL ERRINT ( '#', LAST                                   )
         CALL ERRINT ( '#', FIRST                                  )
         CALL SIGERR ( 'SPICE(REQUESTOUTOFORDER)'                  )
         CALL CHKOUT ( 'SGFREF'                                    )
         RETURN

      END IF
!
!     Ok.  We are ready to go. If the reference type is recognized
!     fetch the requested data.
!
      IF ( MYREFT .EQ. IMPLE ) THEN
!
!        The reference values are implied in this case. Read the
!        reference base value and step. If we fail, check out and
!        return; we don't want to try and comput anything with bogus
!        data.
!
         B = BASE + 1
         E = BASE + 2

         CALL DAFGDA ( HANDLE, B, E, BUFFER )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGFREF' )
            RETURN
         END IF
!
!        Now simply compute the reference values using the implicit
!        model for them.
!
         DO I = FIRST, LAST

            VALUES(I) = BUFFER(1) + DINT ( DBLE(I-1) * BUFFER(2) )

         END DO

      ELSE IF ( MYREFT .EQ. IMPCLS ) THEN
!
!        The reference values are implied in this case. Read the
!        reference base value and step. If we fail, check out and
!        return; we don't want to try and comput anything with bogus
!        data.
!
         B = BASE + 1
         E = BASE + 2

         CALL DAFGDA ( HANDLE, B, E, BUFFER )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGFREF' )
            RETURN
         END IF
!
!        Now simply compute the reference values using the implicit
!        model for them.
!
         DO I = FIRST, LAST

            VALUES(I) = BUFFER(1) + DINT ( DBLE(I-1) * BUFFER(2) )

         END DO

      ELSE IF (      MYREFT .EQ. EXPLE &
                .OR. MYREFT .EQ. EXPLT &
                .OR. MYREFT .EQ. EXPCLS ) THEN
!
!        In this case the reference values are actually stored
!        in the file.  This is even easier than the last case.
!        We simply fetch them with a call to DAF. We do not check for a
!        failure here, since all we do after the attempt to read is
!        checkout and return anyway.
!
         B = BASE + FIRST
         E = BASE + LAST

         CALL DAFGDA ( HANDLE, B, E, VALUES )

      ELSE

         CALL SETMSG ( 'The generic DAF segment you attempted' &
         //            ' to read has an unsupported reference' &
         //            ' directory structure. The integer code' &
         //            ' given for this structure is #. The' &
         //            ' likely cause of this anomaly is that' &
         //            ' your version of SPICELIB needs to be' &
         //            ' updated. Contact your system' &
         //            ' administrator or NAIF for a toolkit' &
         //            ' update. '                                 )
         CALL ERRINT ( '#', MYREFT                                 )
         CALL SIGERR ( 'SPICE(UNKNOWNREFDIR)'                      )
         CALL CHKOUT ( 'SGFREF'                                    )
         RETURN

      END IF

      CALL CHKOUT ( 'SGFREF' )
      RETURN

      END
