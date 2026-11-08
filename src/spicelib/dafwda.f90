!$Procedure DAFWDA ( DAF, write data to address )

      subroutine dafwda ( handle, begin, end, data )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Write or rewrite the double precision data bounded by two
!     addresses within a DAF.
!
! $ Required_Reading
!
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations

      implicit none

      integer(ip)           handle
      integer(ip)           begin
      integer(ip)           end
      real(wp)              data     ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a DAF.
!     BEGIN,
!     END        I   Initial, final address within file.
!     DATA       I   Data to be stored between BEGIN and END.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAF.
!
!     BEGIN,
!     END      are the initial and final addresses of a contiguous
!              set of double precision numbers within a DAF.
!              Presumably, these make up all or part of a
!              particular array.
!
!     DATA     are the double precision data to be stored between
!              the specified addresses within the specified file.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If BEGIN is zero or negative, the error SPICE(DAFNEGADDR)
!         is signaled.
!
!     2)  If the BEGIN > END, the error SPICE(DAFBEGGTEND)
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The principal reason that DAFs are so easy to use is that
!     the data in each DAF are considered to be one long contiguous
!     set of double precision numbers. You can store data anywhere
!     within a DAF without knowing (or caring) about the physical
!     records in which they are stored.
!
!     Of course, if you are merely adding arrays to a DAF,
!     you should not use DAFWDA directly, but should use DAFANA
!     (add new array) and its entry points, since these update
!     the appropriate bookkeeping records automatically.
!
! $ Examples
!
!     The following code fragment illustrates the use of DAFWDA
!     to update an imaginary array. The array begins with a directory
!     containing 11 epochs. Each pair of epochs bounds an
!     interval, and each interval is covered by a set of eight
!     osculating elements.
!
!     By accident, the elements were written with the wrong value for
!     the GM of the central body (the last element in each set). Each
!     set must be retrieved, updated,and rewritten.
!
!        CALL DAFUS ( SUM, ND, NI, DC, IC )
!        BEGIN = IC(5)
!
!        DO I = 1, 10
!           OFFSET = BEGIN + 11 + (I - 1) * 8
!
!           CALL DAFRDA ( HANDLE, OFFSET+1, OFFSET+8, ELEMENTS )
!           ELEMENTS(8) = NEW_GM
!
!           CALL DAFWDA ( HANDLE, OFFSET+1, OFFSET+8, ELEMENTS )
!        END DO
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 27-OCT-2021 (JDR) (NJB)
!
!        Added IMPLICIT NONE statement.
!
!        Local variable BUFFER is now initialized and saved.
!
!        Edited the header to comply with NAIF standard. Moved DAF
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     write data to DAF address
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      integer(ip)           bsize
      parameter           ( bsize = 128 )

      integer(ip)           begr
      integer(ip)           begw
      integer(ip)           endr
      integer(ip)           endw

      integer(ip)           recno
      real(wp)              buffer   ( bsize )
      integer(ip)           first
      integer(ip)           n
      integer(ip)           next
      logical               found

!
!     Saved variables
!
      save                  buffer

!
!     Initial values
!
      data                  buffer / bsize * 0.0_wp /

!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Bad addresses?
!
      if ( begin <= 0 ) then
         call setmsg ( 'Negative beginning address: #' )
         call errint ( '#', begin )
         call sigerr ( 'SPICE(DAFNEGADDR)' )
         return

      else if ( begin > end ) then
         call setmsg ( 'Beginning address (#) greater than ending '   // &
                       'address (#)' )
         call errint ( '#', begin )
         call errint ( '#', end   )
         call sigerr ( 'SPICE(DAFBEGGTEND)' )
         return
      end if

!
!     Convert raw addresses to record/word representations.
!
      call dafarw ( begin, begr, begw )
      call dafarw ( end,   endr, endw )

!
!     The first and last records may have to be read, updated, and
!     rewritten. Any records in between may be written directly.
!
      next = 1

      do recno = begr, endr

         if ( recno == begr  .or.  recno == endr ) then
            call dafrdr ( handle, recno, 1, bsize, buffer, found )

            if ( .not. found ) then
               call cleard ( bsize, buffer )
            end if
         end if

         if ( begr == endr ) then
            first = begw
            n     = endw - begw + 1

         else if ( recno == begr ) then
            first = begw
            n     = bsize - begw + 1

         else if ( recno == endr ) then
            first = 1
            n     = endw

         else
            first = 1
            n     = bsize
         end if

         call moved ( data(next), n, buffer(first) )
         next = next + n

         call dafwdr ( handle, recno, buffer )

      end do

      return
      end
