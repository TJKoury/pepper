!$Procedure      ZZEKECMP ( EK, column entry element comparison )

      integer(ip) function zzekecmp ( hans,  sgdscs,  cldscs,  rows,  elts )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Compare two column entry elements, and return the relation of the
!     first to the second:  LT, EQ, or GT.
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


      include 'ekcoldsc.inc'
      include 'ekopcd.inc'
      include 'ekqlimit.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           hans   ( * )
      integer(ip)           sgdscs ( sdscsz, * )
      integer(ip)           cldscs ( cdscsz, * )
      integer(ip)           rows   ( * )
      integer(ip)           elts   ( * )

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
      character(maxstr)     cval   ( 2 )

      real(wp)              dval   ( 2 )

      integer(ip)           cmplen ( 2 )
      integer(ip)           cvlen  ( 2 )
      integer(ip)           i
      integer(ip)           ival   ( 2 )
      integer(ip)           lhstyp
      integer(ip)           rhstyp

      logical               found
      logical               null   ( 2 )


!
!     Use discovery check-in for speed.
!

!
!     The function value defaults to `equal'.
!
      zzekecmp  =  eq

      lhstyp    =  cldscs( typidx, 1 )
      rhstyp    =  cldscs( typidx, 2 )


      if (  lhstyp == int ) then
!
!        The entities we're comparing are supposed to be
!        scalar.  The left hand side has integer type.  Either
!        integer or double precision types are acceptable on
!        the right hand side.
!
         call zzekrsi (  hans( 1),  sgdscs(1,1),  cldscs(1,1),  rows(1), &
                         elts(1),   ival(1),      null(1),      found  )


         if ( .not. found ) then

            call chkin  ( 'ZZEKECMP'                                  )
            call setmsg ( 'EK = #; COLIDX = #; ROW = #; ELTIDX = #. '// &
                          'Column entry element was not found.'       )
            call errhan ( '#',  hans(1)                               )
            call errint ( '#',  cldscs(ordidx,1)                      )
            call errint ( '#',  rows(1)                               )
            call errint ( '#',  elts(1)                               )
            call sigerr ( 'SPICE(INVALIDINDEX)'                       )
            call chkout ( 'ZZEKECMP'                                  )
            return

         end if


         if ( rhstyp == int ) then


            call zzekrsi ( hans(2), sgdscs(1,2), cldscs(1,2), rows(2), &
                           elts(1), ival(2),     null(2),     found   )

            if ( .not. found ) then

               call chkin  ( 'ZZEKECMP'                                )
               call setmsg ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  // &
                             '= #.Column entry element was not found.' )
               call errhan ( '#',  hans(2)                             )
               call errint ( '#',  cldscs(ordidx,2)                    )
               call errint ( '#',  rows(2)                             )
               call errint ( '#',  elts(2)                             )
               call sigerr ( 'SPICE(INVALIDINDEX)'                     )
               call chkout ( 'ZZEKECMP'                                )
               return

            end if

!
!           Null values precede all others.
!
            if ( null(1) .or. null(2) )  then

               if ( .not. null(2) ) then
                  zzekecmp = lt
               else if ( .not. null(1) ) then
                  zzekecmp = gt
               end if

            else

               if ( ival(1) < ival(2) ) then
                  zzekecmp = lt
               else if ( ival(1) > ival(2) ) then
                  zzekecmp = gt
               end if

            end if


         else if ( rhstyp == dp ) then

            call zzekrsd ( hans(2), sgdscs(1,2), cldscs(1,2), rows(2), &
                           elts(1), dval(2),     null(2),     found   )

            if ( .not. found ) then

               call chkin  ( 'ZZEKECMP'                                )
               call setmsg ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  // &
                             '= #.Column entry element was not found.' )
               call errhan ( '#',  hans(2)                             )
               call errint ( '#',  cldscs(ordidx,2)                    )
               call errint ( '#',  rows(2)                             )
               call errint ( '#',  elts(2)                             )
               call sigerr ( 'SPICE(INVALIDINDEX)'                     )
               call chkout ( 'ZZEKECMP'                                )
               return

            end if


            if ( null(1) .or. null(2) )  then

               if ( .not. null(2) ) then
                  zzekecmp = lt
               else if ( .not. null(1) ) then
                  zzekecmp = gt
               end if

            else

               if ( ival(1) < dval(2) ) then
                  zzekecmp = lt
               else if ( ival(1) > dval(2) ) then
                  zzekecmp = gt
               end if

            end if


         else
!
!           This is a big-time semantic error.  We should
!           never arrive here.
!
            call chkin  ( 'ZZEKECMP'                         )
            call setmsg ( 'LHS data type is #; RHSTYP is #.' )
            call errint ( '#', lhstyp                        )
            call errint ( '#', rhstyp                        )
            call sigerr ( 'SPICE(BUG)'                       )
            call chkout ( 'ZZEKECMP'                         )
            return

         end if



      else if ( lhstyp == dp ) then
!
!        This is a mirror image of the INT case.
!
         call zzekrsd ( hans(1),  sgdscs(1,1),  cldscs(1,1),  rows(1), &
                        elts(1),  dval(1),      null(1),      found   )


         if ( .not. found ) then

            call chkin  ( 'ZZEKECMP'                                  )
            call setmsg ( 'EK = #; COLIDX = #; ROW = #; ELTIDX = #. '// &
                          'Column entry element was not found.'       )
            call errhan ( '#',  hans(1)                               )
            call errint ( '#',  cldscs(ordidx,1)                      )
            call errint ( '#',  rows(1)                               )
            call errint ( '#',  elts(1)                               )
            call sigerr ( 'SPICE(INVALIDINDEX)'                       )
            call chkout ( 'ZZEKECMP'                                  )
            return

         end if


         if ( rhstyp == int ) then

            call zzekrsi ( hans(2), sgdscs(1,2), cldscs(1,2), rows(2), &
                           elts(1), ival(2),     null(2),     found   )

            if ( .not. found ) then

               call chkin  ( 'ZZEKECMP'                                )
               call setmsg ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  // &
                             '= #.Column entry element was not found.' )
               call errhan ( '#',  hans(2)                             )
               call errint ( '#',  cldscs(ordidx,2)                    )
               call errint ( '#',  rows(2)                             )
               call errint ( '#',  elts(2)                             )
               call sigerr ( 'SPICE(INVALIDINDEX)'                     )
               call chkout ( 'ZZEKECMP'                                )
               return

            end if

!
!           Null values precede all others.
!
            if ( null(1) .or. null(2) )  then

               if ( .not. null(2) ) then
                  zzekecmp = lt
               else if ( .not. null(1) ) then
                  zzekecmp = gt
               end if

            else

               if ( dval(1) < ival(2) ) then
                  zzekecmp = lt
               else if ( dval(1) > ival(2) ) then
                  zzekecmp = gt
               end if

            end if


         else if ( rhstyp == dp ) then


            call zzekrsd ( hans(2), sgdscs(1,2), cldscs(1,2), rows(2), &
                           elts(1), dval(2),     null(2),     found   )

            if ( .not. found ) then

               call chkin  ( 'ZZEKECMP'                                )
               call setmsg ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  // &
                             '= #.Column entry element was not found.' )
               call errhan ( '#',  hans(2)                             )
               call errint ( '#',  cldscs(ordidx,2)                    )
               call errint ( '#',  rows(2)                             )
               call errint ( '#',  elts(2)                             )
               call sigerr ( 'SPICE(INVALIDINDEX)'                     )
               call chkout ( 'ZZEKECMP'                                )
               return

            end if


            if ( null(1) .or. null(2) )  then

               if ( .not. null(2) ) then
                  zzekecmp = lt
               else if ( .not. null(1) ) then
                  zzekecmp = gt
               end if

            else

               if ( dval(1) < dval(2) ) then
                  zzekecmp = lt
               else if ( dval(1) > dval(2) ) then
                  zzekecmp = gt
               end if

            end if


         else
!
!           This is a big-time semantic error.  We should
!           never arrive here.
!
            call chkin  ( 'ZZEKECMP'                         )
            call setmsg ( 'LHS data type is #; RHSTYP is #.' )
            call errint ( '#', lhstyp                        )
            call errint ( '#', rhstyp                        )
            call sigerr ( 'SPICE(BUG)'                       )
            call chkout ( 'ZZEKECMP'                         )
            return

         end if



      else if ( lhstyp == time ) then
!
!        The entities we're comparing are supposed to be time values.
!
         if ( rhstyp /= time ) then
!
!           This is a big-time semantic error.  We should
!           never arrive here.
!
            call chkin  ( 'ZZEKECMP'                         )
            call setmsg ( 'LHS data type is #; RHSTYP is #.' )
            call errint ( '#', lhstyp                        )
            call errint ( '#', rhstyp                        )
            call sigerr ( 'SPICE(BUG)'                       )
            call chkout ( 'ZZEKECMP'                         )
            return

         end if


         do i = 1, 2

            call zzekrsd ( hans(i),  sgdscs(1,i), cldscs(1,i), rows(i), &
                           elts(i),  dval(i),     null(i),     found   )


            if ( .not. found ) then

               call chkin  ( 'ZZEKECMP'                                )
               call setmsg ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  // &
                             '= #.Column entry element was not found.' )
               call errhan ( '#',  hans(i)                             )
               call errint ( '#',  cldscs(ordidx,i)                    )
               call errint ( '#',  rows(i)                             )
               call errint ( '#',  elts(i)                             )
               call sigerr ( 'SPICE(INVALIDINDEX)'                     )
               call chkout ( 'ZZEKECMP'                                )
               return

            end if

         end do


         if ( null(1) .or. null(2) )  then

            if ( .not. null(2) ) then
               zzekecmp = lt
            else if ( .not. null(1) ) then
               zzekecmp = gt
            end if

         else

            if ( dval(1) < dval(2) ) then
               zzekecmp = lt
            else if ( dval(1) > dval(2) ) then
               zzekecmp = gt
            end if

         end if


      else if ( lhstyp == chr ) then
!
!        The entities we're comparing are supposed to be scalar.
!
         if ( rhstyp /= chr ) then
!
!           You know what kind of semantic error this is.
!
            call chkin  ( 'ZZEKECMP'                         )
            call setmsg ( 'LHS data type is #; RHSTYP is #.' )
            call errint ( '#', lhstyp                        )
            call errint ( '#', rhstyp                        )
            call sigerr ( 'SPICE(BUG)'                       )
            call chkout ( 'ZZEKECMP'                         )
            return

         end if


         do i = 1, 2

            call zzekrsc (  hans  (  i), &
                            sgdscs(1,i), &
                            cldscs(1,i), &
                            rows(i), &
                            elts(i), &
                            cvlen(i), &
                            cval(i), &
                            null(i), &
                            found           )


            if ( .not. found ) then

               call chkin  ( 'ZZEKECMP'                                )
               call setmsg ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  // &
                             '= #.Column entry element was not found.' )
               call errhan ( '#',  hans(i)                             )
               call errint ( '#',  cldscs(ordidx,i)                    )
               call errint ( '#',  rows(i)                             )
               call errint ( '#',  elts(i)                             )
               call sigerr ( 'SPICE(INVALIDINDEX)'                     )
               call chkout ( 'ZZEKECMP'                                )
               return

            end if

!
!           Let CMPLEN(I) be the string length to use in comparisons.
!
            cmplen(i) = min ( cvlen(i), maxstr )

         end do


         if ( null(1) .or. null(2) )  then

            if ( .not. null(2) ) then
               zzekecmp = lt
            else if ( .not. null(1) ) then
               zzekecmp = gt
            end if

         else


            if (   llt (   cval(1)( :cmplen(1) ), &
                           cval(2)( :cmplen(2) )  )   ) then

               zzekecmp = lt

            else if (   lgt (  cval(1)( :cmplen(1) ), &
                               cval(2)( :cmplen(2) )  )   ) then
               zzekecmp = gt
            else
               zzekecmp = eq
            end if

         end if


      else
!
!        Something untoward has happened in our descriptor.
!
         call chkin  ( 'ZZEKECMP'                        )
         call setmsg ( 'The data type code # was not '  // &
                       'recognized.'                     )
         call errint ( '#',  lhstyp                      )
         call sigerr ( 'SPICE(INVALIDDATATYPE)'          )
         call chkout ( 'ZZEKECMP'                        )
         return

      end if


      return
      end
