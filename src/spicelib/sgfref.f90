!$Procedure SGFREF ( Generic Segments: Fetch references )

      subroutine sgfref ( handle, descr, first, last, values )
      use pepper_kinds, only: ip, wp

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

      implicit none

      integer(ip)           handle
      real(wp)              descr  ( * )
      integer(ip)           first
      integer(ip)           last
      real(wp)              values ( * )

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
      logical               failed
      logical               return

!
!     Local Parameters
!
!     Include the mnemonic values for the generic segment declarations.
!
      include 'sgparam.inc'

!
!     Local Variables
!
      real(wp)              buffer ( 2 )

      integer(ip)           b
      integer(ip)           base
      integer(ip)           e
      integer(ip)           i
      integer(ip)           mynref
      integer(ip)           myreft

      if ( return() ) return

!
!     Perform the needed initialization
!
      call sgmeta ( handle, descr, refbas, base   )
      call sgmeta ( handle, descr, rdrtyp, myreft )
      call sgmeta ( handle, descr, nref,   mynref )

      if ( failed() ) return

!
!     Perform checks on the inputs for reasonableness.
!
      if ( ( first < 1 ) .or. ( last > mynref) ) then

         call setmsg ( 'The range of reference items requested' &
         //            ' extends beyond the available range of' &
         //            ' reference items.  The reference data is' &
         //            ' available' &
         //            ' for indexes 1 to #.  You''ve requested' &
         //            ' data from # to #.'                        )
         call errint ( '#', mynref                                 )
         call errint ( '#', first                                  )
         call errint ( '#', last                                   )
         call sigerr ( 'SPICE(REQUESTOUTOFBOUNDS)'                 )
         return

      end if

      if ( last < first ) then

         call setmsg ( 'The last reference item requested, #, is' &
         //            ' before the first reference item' &
         //            ' requested, #.'                            )
         call errint ( '#', last                                   )
         call errint ( '#', first                                  )
         call sigerr ( 'SPICE(REQUESTOUTOFORDER)'                  )
         return

      end if
!
!     Ok.  We are ready to go. If the reference type is recognized
!     fetch the requested data.
!
      if ( myreft == imple ) then
!
!        The reference values are implied in this case. Read the
!        reference base value and step. If we fail, check out and
!        return; we don't want to try and comput anything with bogus
!        data.
!
         b = base + 1
         e = base + 2

         call dafgda ( handle, b, e, buffer )

         if ( failed() ) return
!
!        Now simply compute the reference values using the implicit
!        model for them.
!
         do i = first, last

            values(i) = buffer(1) + dint ( real(i-1, wp) * buffer(2) )

         end do

      else if ( myreft == impcls ) then
!
!        The reference values are implied in this case. Read the
!        reference base value and step. If we fail, check out and
!        return; we don't want to try and comput anything with bogus
!        data.
!
         b = base + 1
         e = base + 2

         call dafgda ( handle, b, e, buffer )

         if ( failed() ) return
!
!        Now simply compute the reference values using the implicit
!        model for them.
!
         do i = first, last

            values(i) = buffer(1) + dint ( real(i-1, wp) * buffer(2) )

         end do

      else if (      myreft == exple &
                .or. myreft == explt &
                .or. myreft == expcls ) then
!
!        In this case the reference values are actually stored
!        in the file.  This is even easier than the last case.
!        We simply fetch them with a call to DAF. We do not check for a
!        failure here, since all we do after the attempt to read is
!        checkout and return anyway.
!
         b = base + first
         e = base + last

         call dafgda ( handle, b, e, values )

      else

         call setmsg ( 'The generic DAF segment you attempted' &
         //            ' to read has an unsupported reference' &
         //            ' directory structure. The integer code' &
         //            ' given for this structure is #. The' &
         //            ' likely cause of this anomaly is that' &
         //            ' your version of SPICELIB needs to be' &
         //            ' updated. Contact your system' &
         //            ' administrator or NAIF for a toolkit' &
         //            ' update. '                                 )
         call errint ( '#', myreft                                 )
         call sigerr ( 'SPICE(UNKNOWNREFDIR)'                      )
         return

      end if

      return

      end
