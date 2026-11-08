!$Procedure      ZZEKRP2N ( EK, record pointer to number )

      integer(ip) function zzekrp2n ( handle, segno, recptr )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Find the EK record number corresponding to a specified record
!     pointer.  Beware, for type 1 segments, this is done by linear
!     searching.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      include 'eksegdsc.inc'

      integer(ip)           handle
      integer(ip)           segno
      integer(ip)           recptr

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGNO      I   Segment number.
!     RECTPR     I   Record pointer.
!
!     The function returns the number of the record having the
!     specified record pointer.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for read or write
!                    access.
!
!     SEGNO          is the number of the segment containing the
!                    record of interest.
!
!     RECPTR         is a record pointer.  The number of the record
!                    having this pointer is sought.
!
! $ Detailed_Output
!
!     The function returns the number of the record having the
!     specified record pointer.  The record should always be found.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     2)  If SEGNO is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     3)  If an I/O error occurs while reading the indicated file,
!         the error will be diagnosed by routines called by this
!         routine.
!
!     4)  This routine should never be passed an input record pointer
!         that is not known to be valid.  If this error is trapped,
!         it is evidence of a bug.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This searches an EK record tree for the record number
!     corresponding to a specified record pointer.  Caution:  this
!     routine plods along in linear time.  It is intended primarily
!     for use in error handling.
!
! $ Examples
!
!     See EKDELR.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&

!
!     Non-SPICELIB functions
!
      integer(ip)           zzektrls
      logical               failed

!
!     Local variables
!
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           stype

!
!     Use discovery check-in.
!
      zzekrp2n  =  0

      call zzeksdsc ( handle, segno, segdsc )

      if ( failed() ) then
         return
      end if


      stype = segdsc ( ektidx )

      if ( stype .eq. 1 ) then

         zzekrp2n  =  zzektrls ( handle, segdsc(rtidx), recptr )

         if ( zzekrp2n .eq. 0 ) then

            call chkin  ( 'ZZEKRP2N'                              )
            call setmsg ( 'Record having pointer # not found in ' // &
                          'segment # of file #'                   )
            call errint ( '#',  recptr                            )
            call errint ( '#',  segno                             )
            call errhan ( '#',  handle                            )
            call sigerr ( 'SPICE(BUG)'                            )
            call chkout ( 'ZZEKRP2N'                              )

         end if


      else if ( stype .eq. 2 ) then

         zzekrp2n = recptr

      else

         call chkin  ( 'ZZEKRP2N'                                    )
         call setmsg ( 'Segment type # is not supported.  SEGNO = #.'// &
                       ' File = #.'                                  )
         call errint ( '#',  stype                                   )
         call errint ( '#',  segno                                   )
         call errhan ( '#',  handle                                  )
         call sigerr ( 'SPICE(BUG)'                                  )
         call chkout ( 'ZZEKRP2N'                                    )

      end if

      return
      end
