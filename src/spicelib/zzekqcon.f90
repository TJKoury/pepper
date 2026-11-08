!$Procedure  ZZEKQCON ( Private: EK, read constraints from query )

      SUBROUTINE ZZEKQCON ( EQRYI,   EQRYC,   EQRYD,   N, &
                            CNSTYP, &
                            LTNAME,  LTIDX,   LCNAME,  LCIDX, &
                            OPCODE, &
                            RTNAME,  RTIDX,   RCNAME,  RCIDX, &
                            DTYPE, &
                            CBEG,    CEND,    DVAL,    IVAL   )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return elements of a specified constraint from an encoded EK
!     query.
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

      IMPLICIT NONE

      INCLUDE 'ekquery.inc'
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ektype.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               EQRYI ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      DOUBLE PRECISION      EQRYD ( * )
      INTEGER               N
      INTEGER               CNSTYP
      CHARACTER*(*)         LTNAME
      INTEGER               LTIDX
      CHARACTER*(*)         LCNAME
      INTEGER               LCIDX
      INTEGER               OPCODE
      CHARACTER*(*)         RTNAME
      INTEGER               RTIDX
      CHARACTER*(*)         RCNAME
      INTEGER               RCIDX
      INTEGER               DTYPE
      INTEGER               CBEG
      INTEGER               CEND
      DOUBLE PRECISION      DVAL
      INTEGER               IVAL


! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI      I   Integer component of query.
!     EQRYC      I   Character component of query.
!     EQRYD      I   Numeric component of query.
!     N          I   Index of constraint to read.
!     CNSTYP     O   Type of constraint (column or value comparison).
!     LTNAME     O   LHS table name.
!     LTIDX      O   LHS table index in FROM clause.
!     LCNAME     O   LHS column name.
!     LCIDX      O   LHS column index in virtual parent table.
!     OPCODE     O   Operation code.
!     RTNAME     O   RHS table name.
!     RTIDX      O   RHS table index in FROM clause.
!     RCNAME     O   RHS column name.
!     RCIDX      O   RHS column index in virtual parent table.
!     DTYPE      O   Data type of RHS value.
!     CBEG       O   Character begin pointer for RHS value.
!     CEND       O   Character end pointer for RHS value.
!     DVAL       O   RHS double precision value.
!     IVAL       O   RHS integer value.
!
! $ Detailed_Input
!
!     EQRYI,
!     EQRYC,
!     EQRYD          are, respectively, the integer, character, and
!                    numeric components of an encoded EK query.
!                    The query must have names and values resolved and
!                    must have been semantically checked.
!
!     N              is the index, within the WHERE clause of the query,
!                    of the constraint to be fetched.
!
! $ Detailed_Output
!
!     CNSTYP         is the constraint type.  Possible values are
!
!                       EQCOL  ...  constraint compares two columns
!                       EQVAL  ...  constraint compares column and value
!
!     LTNAME         is the table name for the LHS of the constraint.
!                    If an alias was supplied in the query, that
!                    alias is returned.  If the column was unqualified,
!                    LTNAME is returned blank.
!
!     LTIDX          is the index of the LHS table in the FROM clause.
!
!     LCNAME         is the name of the LHS column.
!
!     LCIDX          is the index of the LHS column in the virtual
!                    table containing the column.
!
!     OPCODE         is the operator code used in the constraint.
!
!     RTNAME         is the table name for the RHS of the constraint.
!                    RTNAME is meaningful only if the constraint
!                    compares two columns, as indicated by CNSTYP.
!                    If an alias was supplied in the query, that
!                    alias is returned.  If the column was unqualified,
!                    RTNAME is returned blank.
!
!     RTIDX          is the index of the RHS table in the FROM clause.
!                    RTIDX is meaningful only if the constraint
!                    compares two columns, as indicated by CNSTYP.
!
!     RCNAME         is the name of the RHS column.  RCNAME is
!                    meaningful only if the constraint compares two
!                    columns, as indicated by CNSTYP.
!
!     RCIDX          is the index of the RHS column in the virtual
!                    table containing the column.  RCIDX is
!                    meaningful only if the constraint compares two
!                    columns, as indicated by CNSTYP.
!
!     DTYPE          is the data type of the value on the RHS of the
!                    constraint.  DTYPE is meaningful only if the
!                    constraint compares a column against a value,
!                    as indicated by CNSTYP.
!
!     CBEG,
!     CEND           are, respectively, begin and end character pointers
!                    into the EQRYC array; these pointers give the
!                    location of a character value on the RHS of a
!                    query constraint.  CBEG and CEND are meaningful
!                    only if the constraint compares a column against a
!                    value, as indicated by CNSTYP, and if the value's
!                    data type is CHR, as indicated by DTYPE.
!
!     IVAL           is an integer value on the RHS of the constraint.
!                    IVAL is meaningful only if the constraint compares
!                    a column against a value, as indicated by CNSTYP,
!                    and if the value's data type is INT, as indicated
!                    by DTYPE.
!
!     DVAL           is a double precision value on the RHS of the
!                    constraint.  DVAL is meaningful only if the
!                    constraint compares a column against a value, as
!                    indicated by CNSTYP, and if the value's data type
!                    is DP or TIME, as indicated by DTYPE.
!
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
!     2)  If the input query has not been semantically checked, the
!         error SPICE(NOTSEMCHECKED) will be signaled.  The outputs
!         will not be modified.
!
!     3)  If the index N is less than 1 or greater than the number of
!         constraints in the query, the error SPICE(INVALIDINDEX)
!         will be signaled.  The outputs
!         will not be modified.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The call
!
!        CALL ZZEKREQI (  EQRYI,  'NUM_CONSTRAINTS',  N  )
!
!     may be used to get the constraint count from an encoded query.
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
!-    SPICELIB Version 1.1.0, 29-APR-2009 (NJB)
!
!        Bug fix: this routine now does not attempt to
!        read constraint RHS value parameters from the
!        encoded query when the RHS value is NULL, as
!        indicated by the opcode.
!
!-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Local variables
!
      INTEGER               BASE
      INTEGER               CB
      INTEGER               CE
      INTEGER               ICHECK
      INTEGER               NCNS
      INTEGER               NTAB
      INTEGER               PTR
      INTEGER               TB
      INTEGER               TE

!
!     Use discovery check-in.
!

      CALL ZZEKREQI ( EQRYI, 'SEM_CHECKED', ICHECK )

      IF ( FAILED() ) THEN
         RETURN
      END IF

      IF ( ICHECK .EQ. IFALSE ) THEN

         CALL CHKIN  ( 'ZZEKQCON'                                )
         CALL SETMSG ( 'Encoded query has not been semantically '// &
                       'checked.'                                )
         CALL SIGERR ( 'SPICE(NOTSEMCHECKED)'                    )
         CALL CHKOUT ( 'ZZEKQCON'                                )
         RETURN

      END IF

      CALL ZZEKREQI ( EQRYI, 'NUM_CONSTRAINTS',   NCNS   )
      CALL ZZEKREQI ( EQRYI, 'NUM_TABLES',        NTAB   )


      IF (  ( N .LT. 1 ) .OR. ( N .GT. NCNS )  ) THEN

         CALL CHKIN  ( 'ZZEKQCON'                                      )
         CALL SETMSG ( 'Constraint index # is out of valid range 1:#.' )
         CALL ERRINT ( '#',  N                                         )
         CALL ERRINT ( '#',  NCNS                                      )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                           )
         CALL CHKOUT ( 'ZZEKQCON'                                      )
         RETURN

      END IF

!
!     Compute the base address of the Nth constraint.
!
      BASE   =      EQVBAS &
                  + NTAB  * 2 * EQVDSZ &
                  + (N-1)     * EQCDSZ

!
!     Get the constraint type.
!
      CNSTYP  =  EQRYI ( BASE + EQCTYP )

!
!     Get the LHS items.
!
      LTIDX  =  EQRYI ( BASE + EQLTAB - 1 + EQTORD )
      TB     =  EQRYI ( BASE + EQLTAB - 1 + EQBSTR )
      TE     =  EQRYI ( BASE + EQLTAB - 1 + EQESTR )

      IF ( TB .NE. 0 ) THEN
         LTNAME =  EQRYC ( TB:TE )
      ELSE
         LTNAME = ' '
      END IF

      LCIDX  =  EQRYI ( BASE + EQLCOL - 1 + EQCIDX )
      CB     =  EQRYI ( BASE + EQLCOL - 1 + EQBSTR )
      CE     =  EQRYI ( BASE + EQLCOL - 1 + EQESTR )
      LCNAME =  EQRYC ( CB:CE )

!
!     Next, the opcode.
!
      OPCODE =  EQRYI ( BASE + EQOPCD )

!
!     If the constraint compares two columns, get the RHS table and
!     column info.
!
      IF ( CNSTYP .EQ. EQCOL ) THEN

         RTIDX  =  EQRYI ( BASE + EQRTAB - 1 + EQTORD )
         TB     =  EQRYI ( BASE + EQRTAB - 1 + EQBSTR )
         TE     =  EQRYI ( BASE + EQRTAB - 1 + EQESTR )

         IF ( TB .NE. 0 ) THEN
            RTNAME =  EQRYC ( TB:TE )
         ELSE
            RTNAME = ' '
         END IF

         RCIDX  =  EQRYI ( BASE + EQRCOL - 1 + EQCIDX )
         CB     =  EQRYI ( BASE + EQRCOL - 1 + EQBSTR )
         CE     =  EQRYI ( BASE + EQRCOL - 1 + EQESTR )
         RCNAME =  EQRYC ( CB:CE )

!
!        ...and clear out the scalar outputs.
!
         CBEG   =  1
         CEND   =  1
         DVAL   =  0.D0
         IVAL   =  0


      ELSE
!
!        The constraint compares a column and a value.  Set the
!        appropriate scalar output, and clear out the other outputs.
!
         IF ( ( OPCODE .EQ. ISNULL ) .OR. ( OPCODE .EQ. NOTNUL ) ) THEN
!
!           There's no output value; the opcode implies the value NULL.
!           Set the outputs to innocuous defaults.
!
            CBEG = 1
            CEND = 1
            DVAL = 0.D0
            IVAL = 0

         ELSE
!
!           This is the normal case; set the scalar output values
!           according to the RHS data type.
!
            DTYPE  =  EQRYI ( BASE + EQBVAL - 1 + EQDTYP )

            IF ( DTYPE .EQ. CHR ) THEN

               CBEG   =  EQRYI ( BASE + EQBVAL - 1 + EQBSTR )
               CEND   =  EQRYI ( BASE + EQBVAL - 1 + EQESTR )
               DVAL   =  0.D0
               IVAL   =  0

            ELSE IF ( DTYPE .EQ. INT ) THEN

               PTR    =  EQRYI ( BASE + EQBVAL - 1 + EQVPTR )
               IVAL   =  NINT  ( EQRYD(PTR) )
               DVAL   =  0.D0
               CBEG   =  1
               CEND   =  1

            ELSE
!
!              The data type is DP or TIME.
!
               PTR    =  EQRYI ( BASE + EQBVAL - 1 + EQVPTR )
               DVAL   =  EQRYD ( PTR )
               IVAL   =  0
               CBEG   =  1
               CEND   =  1

            END IF

         END IF

!
!        Set the RHS table and column outputs.
!
         RTIDX  =  0
         RTNAME =  ' '
         RCIDX  =  0
         RTNAME =  ' '

      END IF


      RETURN
      END
