!$Procedure ZZPRSCOR ( Parse aberration correction )

      SUBROUTINE ZZPRSCOR ( ABCORR, ATTBLK )
      IMPLICIT NONE

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

      CHARACTER*(*)         ABCORR
      LOGICAL               ATTBLK ( * )

      INCLUDE 'zzabcorr.inc'

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
      INTEGER               BSRCHC
      LOGICAL               RETURN

!
!     Local variables
!
      CHARACTER*(CORLEN)    CORLST ( NABCOR )
      CHARACTER*(CORLEN)    TMPCOR

      INTEGER               I
      INTEGER               LOC
      INTEGER               ORDVEC ( NABCOR )

      LOGICAL               CONV   ( NABCOR )
      LOGICAL               FIRST
      LOGICAL               GEO    ( NABCOR )
      LOGICAL               LT     ( NABCOR )
      LOGICAL               REL    ( NABCOR )
      LOGICAL               STL    ( NABCOR )
      LOGICAL               XMIT   ( NABCOR )

!
!     Saved variables
!
      SAVE                  CONV
      SAVE                  CORLST
      SAVE                  FIRST
      SAVE                  GEO
      SAVE                  LT
      SAVE                  REL
      SAVE                  STL
      SAVE                  XMIT

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
      DATA &
      ( &
         CORLST(I), GEO(I), LT(I),   STL(I),  CONV(I), XMIT(I), REL(I), &
         I= 1, NABCOR &
      ) / &
       'CN',      .FALSE., .TRUE.,  .FALSE., .TRUE.,  .FALSE., .FALSE., &
       'CN+S',    .FALSE., .TRUE.,  .TRUE.,  .TRUE.,  .FALSE., .FALSE., &
       'LT',      .FALSE., .TRUE.,  .FALSE., .FALSE., .FALSE., .FALSE., &
       'LT+S',    .FALSE., .TRUE.,  .TRUE.,  .FALSE., .FALSE., .FALSE., &
       'NONE',    .TRUE.,  .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., &
       'RL',      .FALSE., .TRUE.,  .FALSE., .FALSE., .FALSE., .TRUE., &
       'RL+S',    .FALSE., .TRUE.,  .TRUE.,  .FALSE., .FALSE., .TRUE., &
       'S',       .FALSE., .FALSE., .TRUE.,  .FALSE., .FALSE., .FALSE., &
       'XCN',     .FALSE., .TRUE.,  .FALSE., .TRUE.,  .TRUE.,  .FALSE., &
       'XCN+S',   .FALSE., .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .FALSE., &
       'XLT',     .FALSE., .TRUE.,  .FALSE., .FALSE., .TRUE.,  .FALSE., &
       'XLT+S',   .FALSE., .TRUE.,  .TRUE.,  .FALSE., .TRUE.,  .FALSE., &
       'XRL',     .FALSE., .TRUE.,  .FALSE., .FALSE., .TRUE.,  .TRUE., &
       'XRL+S',   .FALSE., .TRUE.,  .TRUE.,  .FALSE., .TRUE.,  .TRUE., &
       'XS',      .FALSE., .FALSE., .TRUE.,  .FALSE., .TRUE.,  .FALSE. &
        /


      DATA                  FIRST  / .TRUE. /



      IF ( RETURN() ) THEN
         RETURN
      END IF

      IF ( FIRST ) THEN
!
!        The first time this routine is called, we sort the
!        aberration correction strings and the associated flag
!        lists.  This ensures we have an ordered list suitable
!        for a binary search.
!
!        Find the sorted order of the aberration correction strings.
!
         CALL ORDERC ( CORLST, NABCOR, ORDVEC )

!
!        Put the aberration correction strings and the associated
!        arrays into increasing order.
!
         CALL REORDC ( ORDVEC, NABCOR, CORLST )

         CALL REORDL ( ORDVEC, NABCOR, GEO    )
         CALL REORDL ( ORDVEC, NABCOR, LT     )
         CALL REORDL ( ORDVEC, NABCOR, STL    )
         CALL REORDL ( ORDVEC, NABCOR, CONV   )
         CALL REORDL ( ORDVEC, NABCOR, XMIT   )
         CALL REORDL ( ORDVEC, NABCOR, REL    )

         FIRST = .FALSE.

      END IF

!
!     Obtain a blank-free, upper-case copy of the aberration
!     correction string.
!
      CALL LJUCRS ( 0, ABCORR, TMPCOR )

!
!     Search the list for the aberration correction string.
!
      LOC = BSRCHC ( TMPCOR, NABCOR, CORLST )

      IF ( LOC .EQ. 0 ) THEN

         CALL CHKIN  ( 'ZZPRSCOR' )
         CALL SETMSG ( 'Aberration correction specification # is not '// &
                       'recognized.'                                  )
         CALL ERRCH  ( '#',  ABCORR                                   )
         CALL SIGERR ( 'SPICE(INVALIDOPTION)'                         )
         CALL CHKOUT ( 'ZZPRSCOR'                                     )
         RETURN

      END IF

!
!     Set the output flags.
!
      ATTBLK( GEOIDX )  =  GEO   ( LOC )
      ATTBLK( LTIDX  )  =  LT    ( LOC )
      ATTBLK( STLIDX )  =  STL   ( LOC )
      ATTBLK( CNVIDX )  =  CONV  ( LOC )
      ATTBLK( XMTIDX )  =  XMIT  ( LOC )
      ATTBLK( RELIDX )  =  REL   ( LOC )

      RETURN
      END



