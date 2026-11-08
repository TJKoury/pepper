!$Procedure   ZZEKWEQI ( Private: EK, write to encoded query, integer )

      SUBROUTINE ZZEKWEQI ( NAME, VALUE, EQRYI )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Write scalar integer value to encoded EK query.
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

      INCLUDE 'ekquery.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         NAME
      INTEGER               VALUE
      INTEGER               EQRYI ( LBCELL : * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of scalar item to write.
!     VALUE      I   Value to write.
!     EQRYI     I-O  Integer component of query.
!
! $ Detailed_Input
!
!     NAME           is the name of the item whose value is to be set.
!                    This item is some element of the integer portion
!                    of an encoded query.  The currently supported set
!                    of names is:
!
!                       ARCHITECTURE
!                       INITIALIZED
!                       PARSED
!                       NAMES_RESOLVED
!                       TIMES_RESOLVED
!                       SEM_CHECKED
!                       NUM_TABLES
!                       NUM_CONJUNCTIONS
!                       NUM_CONSTRAINTS
!                       NUM_SELECT_COLS
!                       NUM_ORDERBY_COLS
!                       NUM_BUF_SIZE
!                       FREE_NUM
!                       CHR_BUF_SIZE
!                       FREE_CHR
!
!     VALUE          is an integer value to assign to the quantity
!                    designated by NAME.
!
!     EQRYI          is the integer portion of an encoded EK query.
!
! $ Detailed_Output
!
!     EQRYI          is the integer portion of an encoded EK query,
!                    updated to reflect the requested assignment.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input name is not recognized, the error
!         SPICE(INVALIDNAME) is signaled.  The encoded query is not
!         modified.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is intended to hide from calling routines the
!     specifics of the EK encoded query structure.  See the include
!     file ekquery.inc if details of this structure are desired.
!
! $ Examples
!
!     See ZZEKNRES.
!
! $ Restrictions
!
!     1) Uses EK encoded query architecture version 2.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 01-AUG-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               ISRCHC

!
!     Local parameters
!
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               MAXNAM
      PARAMETER           ( MAXNAM = 15 )
!
!     Local variables
!
      CHARACTER*(NAMLEN)    NAMLST ( MAXNAM )
      CHARACTER*(NAMLEN)    TMPNAM

      INTEGER               I
      INTEGER               NAMIDX ( MAXNAM )

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA  (   NAMLST(I),          NAMIDX(I),   I = 1, MAXNAM  )  / &
                 'ARCHITECTURE',      EQARCH, &
                 'INITIALIZED',       EQINIT, &
                 'PARSED',            EQPARS, &
                 'NAMES_RESOLVED',    EQNRES, &
                 'TIMES_RESOLVED',    EQTRES, &
                 'SEM_CHECKED',       EQSCHK, &
                 'NUM_TABLES',        EQNTAB, &
                 'NUM_CONJUNCTIONS',  EQNCNJ, &
                 'NUM_CONSTRAINTS',   EQNCNS, &
                 'NUM_SELECT_COLS',   EQNSEL, &
                 'NUM_ORDERBY_COLS',  EQNORD, &
                 'NUM_BUF_SIZE',      EQNSIZ, &
                 'FREE_NUM',          EQNPTR, &
                 'CHR_BUF_SIZE',      EQCSIZ, &
                 'FREE_CHR',          EQCPTR                       /


!
!     Use discovery check-in.
!
!
!     Find the location of the named item.
!
      CALL LJUST ( NAME,   TMPNAM )
      CALL UCASE ( TMPNAM, TMPNAM )

      I  =  ISRCHC ( TMPNAM, MAXNAM, NAMLST )

      IF ( I .EQ. 0 ) THEN

         CALL CHKIN  ( 'ZZEKWEQI'           )
         CALL SETMSG ( 'Item # not found.'  )
         CALL ERRCH  ( '#',  NAME           )
         CALL SIGERR ( 'SPICE(INVALIDNAME)' )
         CALL CHKOUT ( 'ZZEKWEQI'           )
         RETURN

      END IF

!
!     Do the deed.
!
      EQRYI( NAMIDX(I) ) = VALUE

      RETURN
      END
