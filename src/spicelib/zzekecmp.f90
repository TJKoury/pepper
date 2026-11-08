!$Procedure      ZZEKECMP ( EK, column entry element comparison )

      INTEGER FUNCTION ZZEKECMP ( HANS,  SGDSCS,  CLDSCS,  ROWS,  ELTS )

! $ Abstract
!
!     Compare two column entry elements, and return the relation of the
!     first to the second:  LT, EQ, or GT.
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
!     EK
!
! $ Keywords
!
!     COMPARE
!     EK
!     UTILITY
!
! $ Declarations


      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANS   ( * )
      INTEGER               SGDSCS ( SDSCSZ, * )
      INTEGER               CLDSCS ( CDSCSZ, * )
      INTEGER               ROWS   ( * )
      INTEGER               ELTS   ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANS       I   EK handles.
!     SGDSCS     I   Segment descriptors.
!     CLDSCS     I   Column descriptors.
!     ROWS       I   Row numbers.
!     ELTS       I   Element indices.
!
!     The function returns a parameter indicating the order relation
!     satisfied by the input arguments.  Possible values are LT, EQ,
!     and GT.
!
! $ Detailed_Input
!
!     HANS           is an array containing file handles of two EKs
!                    containing column entry elements to be compared.
!
!     SGDSCS         is an array containing segment descriptors of
!                    the segments that contain the elements to be
!                    compared.
!
!     CLDSCS         is an array containing column descriptors for the
!                    columns containing the elements to be compared.
!
!     ROWS           is an array containing row numbers of the
!                    elements to be compared.
!
!     ELTS           is an array containing element indices of the
!                    elements to be compared.  These indices locate
!                    an element within the column entry it belongs to.
!
! $ Detailed_Output
!
!     The function returns a parameter indicating the order relation
!     satisfied by the input arguments.  Possible values are LT, EQ,
!     and GT.  If OP is the returned value, the scalar values
!     specified by the input arguments satisfy the relation
!
!        <row 1> OP <row 2>
!
! $ Parameters
!
!     See the include file ekopcd.inc.
!
! $ Exceptions
!
!     1)  If the either of input file handles is invalid, the error
!         will be diagnosed by routines called by this routine.
!         The function value is EQ in this case.
!
!     2)  If an I/O error occurs while attempting to look up
!         the specified column entry elements, the error will
!         be diagnosed by routines called by this routine.  The
!         function value is EQ in this case.
!
!     3)  If any of the input segment descriptors, column descriptors,
!         or row numbers are invalid, this routine may fail in
!         unpredictable, but possibly spectacular, ways.  Except
!         as described in this header section, no attempt is made to
!         handle these errors.
!
!     4)  If the data type code in the input column descriptor is not
!         recognized, the error SPICE(INVALIDDATATYPE) is signaled.
!         The function value is EQ in this case.
!
! $ Files
!
!     See the descriptions of the arguments HAN(1) and HAN(2) in
!     $Detailed_Input.
!
! $ Particulars
!
!     This routine is an EK utility intended to centralize a frequently
!     performed comparison operation.
!
! $ Examples
!
!     See ZZEKRCMP, ZZEKVCMP, ZZEKVMCH.
!
! $ Restrictions
!
!     1)  This routine must execute quickly.  Therefore, it checks in
!         only if it detects an error.  If an error is signaled by a
!         routine called by this routine, this routine will not appear
!         in the SPICELIB traceback display.  Also, in the interest
!         of speed, this routine does not test the value of the SPICELIB
!         function RETURN upon entry.
!
!     2)  This routine depends on the requested comparison to have
!         been semantically checked. Semantically invalid comparisons
!         are treated as bugs.
!
!     3)  Only the first MAXSTR characters of character strings are
!         used in comparisons.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    SPICELIB Version 1.1.0, 26-MAY-2010 (NJB)
!
!        Bug fix: subscript out of range error caused by
!        column entry strings longer than MAXLEN has been
!        corrected. Also updated Restrictions header section.
!
!-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
!
!-&

!
!     Local variables
!
      CHARACTER*(MAXSTR)    CVAL   ( 2 )

      DOUBLE PRECISION      DVAL   ( 2 )

      INTEGER               CMPLEN ( 2 )
      INTEGER               CVLEN  ( 2 )
      INTEGER               I
      INTEGER               IVAL   ( 2 )
      INTEGER               LHSTYP
      INTEGER               RHSTYP

      LOGICAL               FOUND
      LOGICAL               NULL   ( 2 )


!
!     Use discovery check-in for speed.
!

!
!     The function value defaults to `equal'.
!
      ZZEKECMP  =  EQ

      LHSTYP    =  CLDSCS( TYPIDX, 1 )
      RHSTYP    =  CLDSCS( TYPIDX, 2 )


      IF (  LHSTYP .EQ. INT ) THEN
!
!        The entities we're comparing are supposed to be
!        scalar.  The left hand side has integer type.  Either
!        integer or double precision types are acceptable on
!        the right hand side.
!
         CALL ZZEKRSI (  HANS( 1),  SGDSCS(1,1),  CLDSCS(1,1),  ROWS(1), &
                         ELTS(1),   IVAL(1),      NULL(1),      FOUND  )


         IF ( .NOT. FOUND ) THEN

            CALL CHKIN  ( 'ZZEKECMP'                                  )
            CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX = #. '// &
                          'Column entry element was not found.'       )
            CALL ERRHAN ( '#',  HANS(1)                               )
            CALL ERRINT ( '#',  CLDSCS(ORDIDX,1)                      )
            CALL ERRINT ( '#',  ROWS(1)                               )
            CALL ERRINT ( '#',  ELTS(1)                               )
            CALL SIGERR ( 'SPICE(INVALIDINDEX)'                       )
            CALL CHKOUT ( 'ZZEKECMP'                                  )
            RETURN

         END IF


         IF ( RHSTYP .EQ. INT ) THEN


            CALL ZZEKRSI ( HANS(2), SGDSCS(1,2), CLDSCS(1,2), ROWS(2), &
                           ELTS(1), IVAL(2),     NULL(2),     FOUND   )

            IF ( .NOT. FOUND ) THEN

               CALL CHKIN  ( 'ZZEKECMP'                                )
               CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  // &
                             '= #.Column entry element was not found.' )
               CALL ERRHAN ( '#',  HANS(2)                             )
               CALL ERRINT ( '#',  CLDSCS(ORDIDX,2)                    )
               CALL ERRINT ( '#',  ROWS(2)                             )
               CALL ERRINT ( '#',  ELTS(2)                             )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
               CALL CHKOUT ( 'ZZEKECMP'                                )
               RETURN

            END IF

!
!           Null values precede all others.
!
            IF ( NULL(1) .OR. NULL(2) )  THEN

               IF ( .NOT. NULL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( .NOT. NULL(1) ) THEN
                  ZZEKECMP = GT
               END IF

            ELSE

               IF ( IVAL(1) .LT. IVAL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( IVAL(1) .GT. IVAL(2) ) THEN
                  ZZEKECMP = GT
               END IF

            END IF


         ELSE IF ( RHSTYP .EQ. DP ) THEN

            CALL ZZEKRSD ( HANS(2), SGDSCS(1,2), CLDSCS(1,2), ROWS(2), &
                           ELTS(1), DVAL(2),     NULL(2),     FOUND   )

            IF ( .NOT. FOUND ) THEN

               CALL CHKIN  ( 'ZZEKECMP'                                )
               CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  // &
                             '= #.Column entry element was not found.' )
               CALL ERRHAN ( '#',  HANS(2)                             )
               CALL ERRINT ( '#',  CLDSCS(ORDIDX,2)                    )
               CALL ERRINT ( '#',  ROWS(2)                             )
               CALL ERRINT ( '#',  ELTS(2)                             )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
               CALL CHKOUT ( 'ZZEKECMP'                                )
               RETURN

            END IF


            IF ( NULL(1) .OR. NULL(2) )  THEN

               IF ( .NOT. NULL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( .NOT. NULL(1) ) THEN
                  ZZEKECMP = GT
               END IF

            ELSE

               IF ( IVAL(1) .LT. DVAL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( IVAL(1) .GT. DVAL(2) ) THEN
                  ZZEKECMP = GT
               END IF

            END IF


         ELSE
!
!           This is a big-time semantic error.  We should
!           never arrive here.
!
            CALL CHKIN  ( 'ZZEKECMP'                         )
            CALL SETMSG ( 'LHS data type is #; RHSTYP is #.' )
            CALL ERRINT ( '#', LHSTYP                        )
            CALL ERRINT ( '#', RHSTYP                        )
            CALL SIGERR ( 'SPICE(BUG)'                       )
            CALL CHKOUT ( 'ZZEKECMP'                         )
            RETURN

         END IF



      ELSE IF ( LHSTYP .EQ. DP ) THEN
!
!        This is a mirror image of the INT case.
!
         CALL ZZEKRSD ( HANS(1),  SGDSCS(1,1),  CLDSCS(1,1),  ROWS(1), &
                        ELTS(1),  DVAL(1),      NULL(1),      FOUND   )


         IF ( .NOT. FOUND ) THEN

            CALL CHKIN  ( 'ZZEKECMP'                                  )
            CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX = #. '// &
                          'Column entry element was not found.'       )
            CALL ERRHAN ( '#',  HANS(1)                               )
            CALL ERRINT ( '#',  CLDSCS(ORDIDX,1)                      )
            CALL ERRINT ( '#',  ROWS(1)                               )
            CALL ERRINT ( '#',  ELTS(1)                               )
            CALL SIGERR ( 'SPICE(INVALIDINDEX)'                       )
            CALL CHKOUT ( 'ZZEKECMP'                                  )
            RETURN

         END IF


         IF ( RHSTYP .EQ. INT ) THEN

            CALL ZZEKRSI ( HANS(2), SGDSCS(1,2), CLDSCS(1,2), ROWS(2), &
                           ELTS(1), IVAL(2),     NULL(2),     FOUND   )

            IF ( .NOT. FOUND ) THEN

               CALL CHKIN  ( 'ZZEKECMP'                                )
               CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  // &
                             '= #.Column entry element was not found.' )
               CALL ERRHAN ( '#',  HANS(2)                             )
               CALL ERRINT ( '#',  CLDSCS(ORDIDX,2)                    )
               CALL ERRINT ( '#',  ROWS(2)                             )
               CALL ERRINT ( '#',  ELTS(2)                             )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
               CALL CHKOUT ( 'ZZEKECMP'                                )
               RETURN

            END IF

!
!           Null values precede all others.
!
            IF ( NULL(1) .OR. NULL(2) )  THEN

               IF ( .NOT. NULL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( .NOT. NULL(1) ) THEN
                  ZZEKECMP = GT
               END IF

            ELSE

               IF ( DVAL(1) .LT. IVAL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( DVAL(1) .GT. IVAL(2) ) THEN
                  ZZEKECMP = GT
               END IF

            END IF


         ELSE IF ( RHSTYP .EQ. DP ) THEN


            CALL ZZEKRSD ( HANS(2), SGDSCS(1,2), CLDSCS(1,2), ROWS(2), &
                           ELTS(1), DVAL(2),     NULL(2),     FOUND   )

            IF ( .NOT. FOUND ) THEN

               CALL CHKIN  ( 'ZZEKECMP'                                )
               CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  // &
                             '= #.Column entry element was not found.' )
               CALL ERRHAN ( '#',  HANS(2)                             )
               CALL ERRINT ( '#',  CLDSCS(ORDIDX,2)                    )
               CALL ERRINT ( '#',  ROWS(2)                             )
               CALL ERRINT ( '#',  ELTS(2)                             )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
               CALL CHKOUT ( 'ZZEKECMP'                                )
               RETURN

            END IF


            IF ( NULL(1) .OR. NULL(2) )  THEN

               IF ( .NOT. NULL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( .NOT. NULL(1) ) THEN
                  ZZEKECMP = GT
               END IF

            ELSE

               IF ( DVAL(1) .LT. DVAL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( DVAL(1) .GT. DVAL(2) ) THEN
                  ZZEKECMP = GT
               END IF

            END IF


         ELSE
!
!           This is a big-time semantic error.  We should
!           never arrive here.
!
            CALL CHKIN  ( 'ZZEKECMP'                         )
            CALL SETMSG ( 'LHS data type is #; RHSTYP is #.' )
            CALL ERRINT ( '#', LHSTYP                        )
            CALL ERRINT ( '#', RHSTYP                        )
            CALL SIGERR ( 'SPICE(BUG)'                       )
            CALL CHKOUT ( 'ZZEKECMP'                         )
            RETURN

         END IF



      ELSE IF ( LHSTYP .EQ. TIME ) THEN
!
!        The entities we're comparing are supposed to be time values.
!
         IF ( RHSTYP .NE. TIME ) THEN
!
!           This is a big-time semantic error.  We should
!           never arrive here.
!
            CALL CHKIN  ( 'ZZEKECMP'                         )
            CALL SETMSG ( 'LHS data type is #; RHSTYP is #.' )
            CALL ERRINT ( '#', LHSTYP                        )
            CALL ERRINT ( '#', RHSTYP                        )
            CALL SIGERR ( 'SPICE(BUG)'                       )
            CALL CHKOUT ( 'ZZEKECMP'                         )
            RETURN

         END IF


         DO I = 1, 2

            CALL ZZEKRSD ( HANS(I),  SGDSCS(1,I), CLDSCS(1,I), ROWS(I), &
                           ELTS(I),  DVAL(I),     NULL(I),     FOUND   )


            IF ( .NOT. FOUND ) THEN

               CALL CHKIN  ( 'ZZEKECMP'                                )
               CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  // &
                             '= #.Column entry element was not found.' )
               CALL ERRHAN ( '#',  HANS(I)                             )
               CALL ERRINT ( '#',  CLDSCS(ORDIDX,I)                    )
               CALL ERRINT ( '#',  ROWS(I)                             )
               CALL ERRINT ( '#',  ELTS(I)                             )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
               CALL CHKOUT ( 'ZZEKECMP'                                )
               RETURN

            END IF

         END DO


         IF ( NULL(1) .OR. NULL(2) )  THEN

            IF ( .NOT. NULL(2) ) THEN
               ZZEKECMP = LT
            ELSE IF ( .NOT. NULL(1) ) THEN
               ZZEKECMP = GT
            END IF

         ELSE

            IF ( DVAL(1) .LT. DVAL(2) ) THEN
               ZZEKECMP = LT
            ELSE IF ( DVAL(1) .GT. DVAL(2) ) THEN
               ZZEKECMP = GT
            END IF

         END IF


      ELSE IF ( LHSTYP .EQ. CHR ) THEN
!
!        The entities we're comparing are supposed to be scalar.
!
         IF ( RHSTYP .NE. CHR ) THEN
!
!           You know what kind of semantic error this is.
!
            CALL CHKIN  ( 'ZZEKECMP'                         )
            CALL SETMSG ( 'LHS data type is #; RHSTYP is #.' )
            CALL ERRINT ( '#', LHSTYP                        )
            CALL ERRINT ( '#', RHSTYP                        )
            CALL SIGERR ( 'SPICE(BUG)'                       )
            CALL CHKOUT ( 'ZZEKECMP'                         )
            RETURN

         END IF


         DO I = 1, 2

            CALL ZZEKRSC (  HANS  (  I), &
                            SGDSCS(1,I), &
                            CLDSCS(1,I), &
                            ROWS(I), &
                            ELTS(I), &
                            CVLEN(I), &
                            CVAL(I), &
                            NULL(I), &
                            FOUND           )


            IF ( .NOT. FOUND ) THEN

               CALL CHKIN  ( 'ZZEKECMP'                                )
               CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  // &
                             '= #.Column entry element was not found.' )
               CALL ERRHAN ( '#',  HANS(I)                             )
               CALL ERRINT ( '#',  CLDSCS(ORDIDX,I)                    )
               CALL ERRINT ( '#',  ROWS(I)                             )
               CALL ERRINT ( '#',  ELTS(I)                             )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
               CALL CHKOUT ( 'ZZEKECMP'                                )
               RETURN

            END IF

!
!           Let CMPLEN(I) be the string length to use in comparisons.
!
            CMPLEN(I) = MIN ( CVLEN(I), MAXSTR )

         END DO


         IF ( NULL(1) .OR. NULL(2) )  THEN

            IF ( .NOT. NULL(2) ) THEN
               ZZEKECMP = LT
            ELSE IF ( .NOT. NULL(1) ) THEN
               ZZEKECMP = GT
            END IF

         ELSE


            IF (   LLT (   CVAL(1)( :CMPLEN(1) ), &
                           CVAL(2)( :CMPLEN(2) )  )   ) THEN

               ZZEKECMP = LT

            ELSE IF (   LGT (  CVAL(1)( :CMPLEN(1) ), &
                               CVAL(2)( :CMPLEN(2) )  )   ) THEN
               ZZEKECMP = GT
            ELSE
               ZZEKECMP = EQ
            END IF

         END IF


      ELSE
!
!        Something untoward has happened in our descriptor.
!
         CALL CHKIN  ( 'ZZEKECMP'                        )
         CALL SETMSG ( 'The data type code # was not '  // &
                       'recognized.'                     )
         CALL ERRINT ( '#',  LHSTYP                      )
         CALL SIGERR ( 'SPICE(INVALIDDATATYPE)'          )
         CALL CHKOUT ( 'ZZEKECMP'                        )
         RETURN

      END IF


      RETURN
      END
