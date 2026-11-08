!$Procedure  ZZEKTRES ( Private: EK, resolve times in encoded query )

      SUBROUTINE ZZEKTRES ( QUERY,  EQRYI,  EQRYC,   EQRYD, &
                                    ERROR,  ERRMSG,  ERRPTR  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Resolve time values in an encoded EK query.
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
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      INCLUDE 'ekattdsc.inc'
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         QUERY
      INTEGER               EQRYI  ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      DOUBLE PRECISION      EQRYD  ( * )
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG
      INTEGER               ERRPTR

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI     I-O  Integer component of query.
!     EQRYC     I-O  Character component of query.
!     EQRYD     I-O  Numeric component of query.
!     ERROR      O   Error flag.
!     ERRMSG     O   Error message.
!     ERRPTR     O   Position in query where error was detected.
!
! $ Detailed_Input
!
!     QUERY          is the original query from which EQRYI and EQRYC
!                    were obtained.  QUERY is used only for
!                    construction of error messages.
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed.
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     EQRYD          is the numeric portion of an encoded EK query.
!
! $ Detailed_Output
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    On output, all valid time values will have been
!                    converted from strings to equivalent numeric
!                    values which represent times as ephemeris
!                    seconds past J2000 (TDB).
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     ERROR          is a logical flag indicating whether an error was
!                    detected.  Note that a time string might be
!                    syntactically valid, but incapable of being
!                    converted to ET if the appropriate time kernels
!                    (Leapseconds or SCLK) are not loaded.
!
!     ERRMSG         is an error message describing an error in the
!                    input query, if one was detected.  If ERROR is
!                    returned .FALSE., then ERRPTR is undefined.
!
!     ERRPTR         is the character position in the original query
!                    at which an error was detected, if an error was
!                    found.  This index refers to the offending lexeme's
!                    position in the original query represented by the
!                    input encoded query.  If ERROR is returned .FALSE.,
!                    ERRPTR is undefined.
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input query is not initialized, the error will be
!         diagnosed by routines called by this routine.  The outputs
!         will not be modified.
!
!     2)  If names have not been resolved in the input query, the error
!         SPICE(NAMESNOTRESOLVED) will be signaled.  The outputs
!         will not be modified.
!
!     3)  If any sort of time conversion error occurs, the output flag
!         ERROR is set, and an error message is returned.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Strings representing time values are interpreted as follows:
!
!        1)  The string is first examined to see whether it's an
!            SCLK string for a recognized clock; if it is, the
!            string is converted to the equivalent ET.
!
!        2)  If the string is not a SCLK string, it is expected
!            to be some sort of UTC representation.  The string is
!            checked to see whether it's an ISO format UTC time that
!            ISO2UTC can handle.
!
!        3)  If the string does not conform to an ISO format, the
!            last chance is to try to get the string through
!            TPARSE.  If TPARSE can't deal with it, it's considered
!            to be invalid.
!
!
!     This routine assumes that encoded EK query architecture version
!     1 is to be used with the query to be initialized; this routine
!     will not work with any other architecture version.
!
! $ Examples
!
!     See EKFIND.
!
! $ Restrictions
!
!     1) A leapseconds kernel must be loaded at the time this routine
!        is called.
!
!     2) In order to convert SCLK strings, an appropriate SCLK kernel
!        must be loaded at the time this routine is called.
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
!-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Local parameters
!
      INTEGER               SHORT
      PARAMETER           ( SHORT  = 32 )

!
!     Local variables
!
      CHARACTER*(TNAMSZ)    ALIAS  ( MAXTAB )
      CHARACTER*(CNAMSZ)    COLNAM
      CHARACTER*(TNAMSZ)    TABLE  ( MAXTAB )
      CHARACTER*(SHORT)     TIMSTR
      CHARACTER*(1)         TOUCHC

      DOUBLE PRECISION      ET

      INTEGER               ATTDSC ( ADSCSZ )
      INTEGER               BASE
      INTEGER               CNSTYP
      INTEGER               COLIDX
      INTEGER               DESCR  ( EQVDSZ )
      INTEGER               DTYPE
      INTEGER               I
      INTEGER               IRSOLV
      INTEGER               LXB
      INTEGER               LXE
      INTEGER               NCNS
      INTEGER               NTAB
      INTEGER               OPCODE
      INTEGER               SB
      INTEGER               SE
      INTEGER               TABIDX

!
!     No error to start with.
!
      ERROR  =  .FALSE.
      ERRMSG =  ' '
      ERRPTR =  0

      TOUCHC =  QUERY(1:1)

!
!     The query must have had names resolved at this point, or it's no
!     go.
!
      CALL ZZEKREQI ( EQRYI, 'NAMES_RESOLVED', IRSOLV )

      IF ( FAILED() ) THEN
         RETURN
      END IF

      IF ( IRSOLV .EQ. IFALSE ) THEN

         CALL CHKIN  ( 'ZZEKTRES'                                  )
         CALL SETMSG ( 'Names are not resolved in encoded query.'  )
         CALL SIGERR ( 'SPICE(NAMESNOTRESOLVED)'                   )
         CALL CHKOUT ( 'ZZEKTRES'                                  )
         RETURN

      END IF

!
!     Time strings occur only on the right sides of constraints.
!     Examine each constraint that compares a column and a value.
!
      CALL ZZEKREQI ( EQRYI, 'NUM_TABLES',      NTAB )
      CALL ZZEKREQI ( EQRYI, 'NUM_CONSTRAINTS', NCNS )

      DO I = 1, NCNS
!
!        Calculate the base address of the constraint.
!
         BASE    =  EQVBAS + (NTAB * 2 * EQVDSZ) + (I-1)*EQCDSZ

!
!        Obtain the constraint type.  If the RHS is not a value or if
!        the RHS is null (as indicated by the opcode), we can skip it.
!
         CNSTYP  =  EQRYI ( BASE + EQCTYP )
         OPCODE  =  EQRYI ( BASE + EQOPCD )

         IF (        ( CNSTYP .EQ. EQVAL  ) &
              .AND.  ( OPCODE .NE. ISNULL ) &
              .AND.  ( OPCODE .NE. NOTNUL )  ) THEN
!
!           Get the index of the table containing the LHS column, and
!           get the index of this column within that table.  Get the
!           table name, then get the column's attributes.
!
            TABIDX  =  EQRYI ( BASE + EQLTAB - 1 + EQTORD )
            COLIDX  =  EQRYI ( BASE + EQLCOL - 1 + EQCIDX )

            CALL ZZEKQTAB ( EQRYI, EQRYC, TABIDX, TABLE, ALIAS )

            CALL EKCII ( TABLE, COLIDX, COLNAM, ATTDSC )

            DTYPE  =  ATTDSC(2)


            IF ( DTYPE .EQ. TIME ) THEN
!
!              The RHS points to a string representing a time
!              value.
!
               LXB    =  EQRYI ( BASE + EQBVAL - 1 + EQBLEX )
               LXE    =  EQRYI ( BASE + EQBVAL - 1 + EQBLEX )
               SB     =  EQRYI ( BASE + EQBVAL - 1 + EQBSTR )
               SE     =  EQRYI ( BASE + EQBVAL - 1 + EQESTR )
               TIMSTR =  EQRYC ( SB:SE )

!
!              Convert the time to ET, if possible.
!
               CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )

               IF ( ERROR ) THEN

                  ERRPTR = SB
                  RETURN

               END IF
!
!              Insert the ET value into the query, and replace the
!              value descriptor for the time string.
!
               CALL ZZEKINQN ( ET, TIME, LXB, LXE, EQRYI, EQRYD, DESCR )

               CALL MOVEI ( DESCR, EQVDSZ, EQRYI(BASE+EQBVAL) )

            END IF
!
!           We've parsed a time string, if the current column's type
!           was TIME.
!

         END IF
!
!        We've examined the current constraint, if it compares a
!        column with a value.
!

      END DO

!
!     Indicate completion of time resolution.
!
      CALL ZZEKWEQI (  'TIMES_RESOLVED',  ITRUE,  EQRYI  )


      RETURN
      END
