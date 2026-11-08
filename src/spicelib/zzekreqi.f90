!$Procedure   ZZEKREQI ( Private: EK, read from encoded query, integer )

      SUBROUTINE ZZEKREQI ( EQRYI, NAME, VALUE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read scalar integer value from encoded EK query.
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

      INTEGER               EQRYI ( LBCELL : * )
      CHARACTER*(*)         NAME
      INTEGER               VALUE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI      I   Integer component of query.
!     NAME       I   Name of scalar item to read.
!     VALUE      O   Value of item.
!
! $ Detailed_Input
!
!     EQRYI          is the integer portion of an encoded EK query.
!
!     NAME           is the name of the item whose value is to be read.
!                    This item is some element of the integer portion
!                    of an encoded query.
!
! $ Detailed_Output
!
!     VALUE          is the integer value designated by NAME.
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
!     This routine is the inverse of ZZEKWEQI.
!
! $ Examples
!
!     See EKSRCH.
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
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
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

         CALL CHKIN  ( 'ZZEKREQI'           )
         CALL SETMSG ( 'Item # not found.'  )
         CALL ERRCH  ( '#',  NAME           )
         CALL SIGERR ( 'SPICE(INVALIDNAME)' )
         CALL CHKOUT ( 'ZZEKREQI'           )
         RETURN

      END IF

!
!     Do the deed.
!
      VALUE  =  EQRYI( NAMIDX(I) )

      RETURN
      END
