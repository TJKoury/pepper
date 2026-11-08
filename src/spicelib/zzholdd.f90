!$Procedure ZZHOLDD ( Private --- hold a scalar DP )

      SUBROUTINE ZZHOLDD ( OP, ID, OK, VALUE )

! $ Abstract
!
!     SPICE private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Persistently store double precision values or retrieve stored
!     double precision values. That's it, not really rocket science.
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
!     None.
!
! $ Keywords
!
!     STORE DP VALUE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'zzholdd.inc'

      INTEGER               OP
      INTEGER               ID
      LOGICAL               OK
      DOUBLE PRECISION      VALUE

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
      INTEGER               BRCKTI

      LOGICAL               RETURN

!
!     Local variables.
!

      DOUBLE PRECISION      SVALUE  (NID)
      LOGICAL               INIT
      LOGICAL               FIRST   (NID)
      INTEGER               I

      SAVE                  SVALUE
      SAVE                  FIRST
      SAVE                  INIT

      DATA                  INIT  / .TRUE. /

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         VALUE = 0.D0
         OK    = .FALSE.
         RETURN
      END IF


!
!     Confirm a proper ID value.
!
      IF ( BRCKTI(ID, 1, NID) .NE. ID ) THEN

         VALUE = 0.D0
         OK    = .FALSE.

         CALL CHKIN  ( 'ZZHOLDD'                               )
         CALL SETMSG ( 'ID value unknown. ID value #1 ' &
               //      'not an element of [1, #2]. Confirm' &
               //      'the ID value exists in the zzholdd.inc ' &
               //      'parameter file.'                       )
         CALL ERRINT ( '#1', ID                                )
         CALL ERRINT ( '#2', NID                               )
         CALL SIGERR ( 'SPICE(UNKNOWNID)'                      )
         CALL CHKOUT ( 'ZZHOLDD'                               )
         RETURN

      END IF


!
!     Initialize the FIRST array; perform once per program run.
!
      IF ( INIT ) THEN

         DO I = 1, NID
            FIRST( I ) = .TRUE.
         END DO

         INIT = .FALSE.

      END IF


!
!     Perform the operation as described by OP.
!
      IF ( OP .EQ. ZZGET ) THEN

!
!        Attempt to retrieve a stored double precision value for ID.
!
!          - Return the value stored by a put operation and OK
!            as true.
!
!          - If no previous set to this ID, return value as zero and
!            OK as false.
!

         IF ( FIRST( ID ) ) THEN

            VALUE = 0.D0
            OK    = .FALSE.

         ELSE

!
!           Return the stored value.
!
            VALUE = SVALUE( ID )
            OK    = .TRUE.

         END IF


      ELSE IF ( OP .EQ. ZZPUT ) THEN

!
!        Store a value for later use. Set FIRST to false
!        so subsequent get calls will work.
!
         IF ( FIRST( ID ) ) THEN

            FIRST( ID ) = .FALSE.

         END IF

         SVALUE( ID ) = VALUE

      ELSE IF ( OP .EQ. ZZRESET ) THEN

!
!        Reset FIRST( ID ) forcing a put before a get.
!
         FIRST( ID ) = .TRUE.

      ELSE

!
!        Unknown value for 'OP'. Signal an error.
!
         VALUE = 0.D0
         OK    = .FALSE.

         CALL CHKIN  ( 'ZZHOLDD'                          )
         CALL SETMSG ( 'Unknown operation. Confirm the OP ' &
               //      'value # exists in the zzholdd.inc ' &
               //      'parameter file.'                  )
         CALL ERRINT ( '#', OP                            )
         CALL SIGERR ( 'SPICE(UNKNOWNOP)'                 )
         CALL CHKOUT ( 'ZZHOLDD'                          )
         RETURN

      END IF

      END

