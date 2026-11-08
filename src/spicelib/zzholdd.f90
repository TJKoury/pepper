!$Procedure ZZHOLDD ( Private --- hold a scalar DP )

      subroutine zzholdd ( op, id, ok, value )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Persistently store double precision values or retrieve stored
!     double precision values. That's it, not really rocket science.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     STORE DP VALUE
!
! $ Declarations

      implicit none

      include               'zzholdd.inc'

      integer(ip)           op
      integer(ip)           id
      logical               ok
      real(wp)              value

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     OP         I   Key for operation to execute.
!     ID         I   The ID for the item to apply OP.
!     OK         O   Boolean indicating success of get operation.
!     VALUE     I-O  Double precision value returned or to store.
!
! $ Detailed_Input
!
!     OP          The scalar integer key for the operation to execute.
!                 Proper values of OP:
!
!                    ZZPUT     store a double precision value for
!                              later use (put).
!
!                    ZZGET     retrieve a stored double precision
!                              value (get).
!
!                    ZZRESET   reset function to require a ZZPUT prior
!                              to a subsequent ZZGET (clear).
!
!     ID          The scalar integer ID for the item to get/put etc.
!                 Proper values of ID:
!
!                    GEN       general value, primarily for testing.
!
!                    GF_REF    user defined GF reference value.
!
!                    GF_TOL    user defined GF convergence tolerance.
!
!                    GF_DT     user defined GF step for numeric
!                              differentiation.
!
!     VALUE       The scalar double precision value to store (put).
!
!     The include file "zzholdd.inc" lists all accepted values for
!     ID and OP.
!
! $ Detailed_Output
!
!     OK          The logical flag indicating if a get operation
!                 returned a valid value for ID. OK returns false if a
!                 get operation occurs before a put.
!
!                 This argument has no meaning except when performing
!                 a get operation.
!
!     VALUE       The scalar double precision value retrieved (get).
!
! $ Parameters
!
!    None.
!
! $ Exceptions
!
!     1)  The error SPICE(UNKNOWNID) signals if the value of ID is
!         not one of those coded in zzholdd.inc.
!
!     2)  The error SPICE(UNKNOWNOP) signals if the value of OP is
!         not one of those coded in zzholdd.inc.
!
! $ Files
!
!    zzholdd.inc
!
! $ Particulars
!
!     This routine simply stores double precision values for later
!     retrieval.
!
!     A get operation may succeed or fail based on whether
!     a put operation preceded the put.
!
!        A ZZHOLDD get operation for an ID called before a put operation
!        for that ID returns with OK as false, VALUE as 0.
!
!        A ZZHOLDD get operation for an ID called after a put operation
!        for that ID returns with OK as true, VALUE as the value
!        assigned by the put.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     Store values using ZZHOLDD then attempt to retrieve the values.
!
!           PROGRAM ZZHOLDD_T
!           IMPLICIT NONE
!
!           INCLUDE 'zzholdd.inc'
!
!           DOUBLE PRECISION     VALUE
!           DOUBLE PRECISION     X
!           DOUBLE PRECISION     Y
!           DOUBLE PRECISION     Z
!           LOGICAL              OK
!
!           X = -11.D0
!           Y =  22.D0
!           Z = -33.D0
!
!     C
!     C     Perform a put then get.
!     C
!           VALUE = 0.D0
!           OK    = .FALSE.
!           CALL ZZHOLDD ( ZZPUT, GEN, OK, X)
!           CALL ZZHOLDD ( ZZGET, GEN, OK, VALUE )
!
!           IF (OK) THEN
!              WRITE(*,*) 'Check 1 ', VALUE
!           ELSE
!              WRITE(*,*) 'Error 1 '
!           END IF
!
!     C
!     C     Reset then get without put.
!     C
!           VALUE = 0.D0
!           OK    = .FALSE.
!
!           CALL ZZHOLDD ( ZZRESET,   GEN, OK, VALUE )
!           CALL ZZHOLDD ( ZZGET,     GEN, OK, VALUE )
!
!           IF (OK) THEN
!              WRITE(*,*) 'Error 2 '
!           ELSE
!              WRITE(*,*) 'Check 2 ', VALUE
!           END IF
!
!     C
!     C     Now put.
!     C
!           CALL ZZHOLDD ( ZZPUT, GEN, OK, Y)
!           CALL ZZHOLDD ( ZZGET, GEN, OK, VALUE )
!
!           IF (OK) THEN
!              WRITE(*,*) 'Check 3 ', VALUE
!           ELSE
!              WRITE(*,*) 'Error 3 '
!           END IF
!
!
!     C
!     C     Now another put with a different value.
!     C
!           CALL ZZHOLDD ( ZZPUT, GEN, OK, Z)
!           CALL ZZHOLDD ( ZZGET, GEN, OK, VALUE )
!
!           IF (OK) THEN
!              WRITE(*,*) 'Check 4 ', VALUE
!           ELSE
!              WRITE(*,*) 'Error 4 '
!           END IF
!
!           END
!
!   The program outputs:
!
!      Check 1   -11.000000000000000
!      Check 2    0.0000000000000000
!      Check 3    22.000000000000000
!      Check 4   -33.000000000000000
!
!    As expected.
!
! $ Restrictions
!
!    None.
!
! $ Literature_References
!
!    None.
!
! $ Author_and_Institution
!
!    E.D. Wright    (JPL)
!
! $ Version
!
!-   SPICELIB Version 1.2.0  07-OCT-2021 (EDW)
!
!      Added explicit output argument assignments for
!      return-on-entry case.
!
!-   SPICELIB Version 1.1.0  03-DEC-2013 (EDW)
!
!       Added ID and OK arguments to routine, generalizing use.
!
!       Added RETURN() check.
!
!-   SPICELIB Version 1.0.0  16-FEB-2010 (EDW)
!
!-&

! $ Index_Entries
!
!    store a double precision value
!    retrieve a stored double precision value
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           brckti

      logical               return

!
!     Local variables.
!

      real(wp)              svalue  (nid)
      logical               init
      logical               first   (nid)
      integer(ip)           i

      save                  svalue
      save                  first
      save                  init

      data                  init  / .true. /

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         value = 0.0_wp
         ok    = .false.
         return
      end if


!
!     Confirm a proper ID value.
!
      if ( brckti(id, 1, nid) .ne. id ) then

         value = 0.0_wp
         ok    = .false.

         call chkin  ( 'ZZHOLDD'                               )
         call setmsg ( 'ID value unknown. ID value #1 ' &
               //      'not an element of [1, #2]. Confirm' &
               //      'the ID value exists in the zzholdd.inc ' &
               //      'parameter file.'                       )
         call errint ( '#1', id                                )
         call errint ( '#2', nid                               )
         call sigerr ( 'SPICE(UNKNOWNID)'                      )
         call chkout ( 'ZZHOLDD'                               )
         return

      end if


!
!     Initialize the FIRST array; perform once per program run.
!
      if ( init ) then

         do i = 1, nid
            first( i ) = .true.
         end do

         init = .false.

      end if


!
!     Perform the operation as described by OP.
!
      if ( op .eq. zzget ) then

!
!        Attempt to retrieve a stored double precision value for ID.
!
!          - Return the value stored by a put operation and OK
!            as true.
!
!          - If no previous set to this ID, return value as zero and
!            OK as false.
!

         if ( first( id ) ) then

            value = 0.0_wp
            ok    = .false.

         else

!
!           Return the stored value.
!
            value = svalue( id )
            ok    = .true.

         end if


      else if ( op .eq. zzput ) then

!
!        Store a value for later use. Set FIRST to false
!        so subsequent get calls will work.
!
         if ( first( id ) ) then

            first( id ) = .false.

         end if

         svalue( id ) = value

      else if ( op .eq. zzreset ) then

!
!        Reset FIRST( ID ) forcing a put before a get.
!
         first( id ) = .true.

      else

!
!        Unknown value for 'OP'. Signal an error.
!
         value = 0.0_wp
         ok    = .false.

         call chkin  ( 'ZZHOLDD'                          )
         call setmsg ( 'Unknown operation. Confirm the OP ' &
               //      'value # exists in the zzholdd.inc ' &
               //      'parameter file.'                  )
         call errint ( '#', op                            )
         call sigerr ( 'SPICE(UNKNOWNOP)'                 )
         call chkout ( 'ZZHOLDD'                          )
         return

      end if

      end

