!$Procedure ZZPRSCOR ( Parse aberration correction )

      subroutine zzprscor ( abcorr, attblk )
      use pepper_kinds, only: ip
      implicit none

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Parse an aberration correction string; return attributes.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ABERRATION
!     PARSING
!     PRIVATE
!     UTILITY
!
! $ Declarations

      character(*)          abcorr
      logical               attblk ( * )

      include 'zzabcorr.inc'

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     ABCORR     I   Aberration correction string.
!     ATTBLK     O   Aberration correction attribute block.
!
! $ Detailed_Input
!
!     ABCORR         is a string representing a aberration
!                    correction.  The supported values are:
!
!                       'CN'
!                       'CN+S'
!                       'LT'
!                       'LT+S'
!                       'NONE'
!                       'RL'
!                       'RL+S'
!                       'S'
!                       'XCN'
!                       'XCN+S'
!                       'XLT'
!                       'XLT+S'
!                       'XRL'
!                       'XRL+S'
!                       'XS'
!
!                    Note that some values not supported by the
!                    SPICELIB SPK subsystem are supported by
!                    this routine:
!
!                       - The letter 'R' indicates relativistic
!                         corrections.
!
!                       - Stellar aberration-only corrections are
!                         indicated by the strings
!
!                            'S'
!                            'XS'
!
!                    Case and embedded blanks are not significant in
!                    ABCORR.
!
! $ Detailed_Output
!
!     ATTBLK         is a block of logical flags indicating the
!                    attributes of the aberration correction
!                    specified by ABCORR.  The attributes are:
!
!                       - Is the correction "geometric"?
!
!                       - Is light time correction indicated?
!
!                       - Is stellar aberration correction indicated?
!
!                       - Is the light time correction of the
!                         "converged Newtonian" variety?
!
!                       - Is the correction for the transmission
!                         case?
!
!                       - Is the correction relativistic?
!
!                    The structure of ATTBLK is defined in the
!                    include file
!
!                       zzabcorr.inc
!
!                    The size of ATTBLK and the offsets of the
!                    component flags are defined there.
!
! $ Parameters
!
!     See INCLUDE file zzabcorr.inc.
!
! $ Exceptions
!
!     1) If the input aberration correction choice is not recognized,
!        the error SPICE(INVALIDOPTION) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Many SPICELIB routines have logic branches based on the
!     attributes of aberration corrections.  Much duplicated
!     parsing code can be avoided by using this routine.
!
!     In particular, the routine ZZCOREPC uses this routine
!     to combine an epoch and light time value to compute
!     a light-time-adjusted epoch.
!
! $ Examples
!
!     See ZZCOREPC.
!
! $ Restrictions
!
!     1) This is a SPICE private routine; the routine is subject
!        to change without notice.  User applications should not
!        call this routine.
!
!     2) This routine recognizes some aberration corrections not
!        handled by most SPICELIB routines.  Callers should do
!        their own checking to ensure the parsed correction is
!        acceptable.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 21-SEP-2013 (BVS)
!
!        Efficiency updates: moved CHKIN inside the exception block;
!        replaced CMPRSS/UCASE with LJUCRS.
!
!-    SPICELIB Version 1.0.0, 13-DEC-2004 (NJB)
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           bsrchc
      logical               return

!
!     Local variables
!
      character(corlen)     corlst ( nabcor )
      character(corlen)     tmpcor

      integer(ip)           i
      integer(ip)           loc
      integer(ip)           ordvec ( nabcor )

      logical               conv   ( nabcor )
      logical               first
      logical               geo    ( nabcor )
      logical               lt     ( nabcor )
      logical               rel    ( nabcor )
      logical               stl    ( nabcor )
      logical               xmit   ( nabcor )

!
!     Saved variables
!
      save                  conv
      save                  corlst
      save                  first
      save                  geo
      save                  lt
      save                  rel
      save                  stl
      save                  xmit

!
!     Initial values
!
!
!     It is recommended that, for maintainability, the correction
!     strings be kept in increasing order in this list.  However,
!     this routine does not rely on the strings being ordered
!     in this data statement:  the strings and associated values
!     are ordered at run time.
!
      data &
      ( &
         corlst(i), geo(i), lt(i),   stl(i),  conv(i), xmit(i), rel(i), &
         i= 1, nabcor &
      ) / &
       'CN',      .false., .true.,  .false., .true.,  .false., .false., &
       'CN+S',    .false., .true.,  .true.,  .true.,  .false., .false., &
       'LT',      .false., .true.,  .false., .false., .false., .false., &
       'LT+S',    .false., .true.,  .true.,  .false., .false., .false., &
       'NONE',    .true.,  .false., .false., .false., .false., .false., &
       'RL',      .false., .true.,  .false., .false., .false., .true., &
       'RL+S',    .false., .true.,  .true.,  .false., .false., .true., &
       'S',       .false., .false., .true.,  .false., .false., .false., &
       'XCN',     .false., .true.,  .false., .true.,  .true.,  .false., &
       'XCN+S',   .false., .true.,  .true.,  .true.,  .true.,  .false., &
       'XLT',     .false., .true.,  .false., .false., .true.,  .false., &
       'XLT+S',   .false., .true.,  .true.,  .false., .true.,  .false., &
       'XRL',     .false., .true.,  .false., .false., .true.,  .true., &
       'XRL+S',   .false., .true.,  .true.,  .false., .true.,  .true., &
       'XS',      .false., .false., .true.,  .false., .true.,  .false. &
        /


      data                  first  / .true. /



      if ( return() ) return

      if ( first ) then
!
!        The first time this routine is called, we sort the
!        aberration correction strings and the associated flag
!        lists.  This ensures we have an ordered list suitable
!        for a binary search.
!
!        Find the sorted order of the aberration correction strings.
!
         call orderc ( corlst, nabcor, ordvec )

!
!        Put the aberration correction strings and the associated
!        arrays into increasing order.
!
         call reordc ( ordvec, nabcor, corlst )

         call reordl ( ordvec, nabcor, geo    )
         call reordl ( ordvec, nabcor, lt     )
         call reordl ( ordvec, nabcor, stl    )
         call reordl ( ordvec, nabcor, conv   )
         call reordl ( ordvec, nabcor, xmit   )
         call reordl ( ordvec, nabcor, rel    )

         first = .false.

      end if

!
!     Obtain a blank-free, upper-case copy of the aberration
!     correction string.
!
      call ljucrs ( 0, abcorr, tmpcor )

!
!     Search the list for the aberration correction string.
!
      loc = bsrchc ( tmpcor, nabcor, corlst )

      if ( loc == 0 ) then

         call setmsg ( 'Aberration correction specification # is not '// &
                       'recognized.'                                  )
         call errch  ( '#',  abcorr                                   )
         call sigerr ( 'SPICE(INVALIDOPTION)'                         )
         return

      end if

!
!     Set the output flags.
!
      attblk( geoidx )  =  geo   ( loc )
      attblk( ltidx  )  =  lt    ( loc )
      attblk( stlidx )  =  stl   ( loc )
      attblk( cnvidx )  =  conv  ( loc )
      attblk( xmtidx )  =  xmit  ( loc )
      attblk( relidx )  =  rel   ( loc )

      return
      end



