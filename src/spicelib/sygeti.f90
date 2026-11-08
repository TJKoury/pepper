!$Procedure SYGETI ( Return all components for a symbol )

      SUBROUTINE SYGETI ( NAME, &
                          TABSYM, TABPTR, TABVAL, &
                          N,      VALUES, &
                          FOUND                     )

! $ Abstract
!
!     Return the dimension and associated values for a particular
!     symbol.
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
!     SYMBOLS
!
! $ Keywords
!
!     SYMBOLS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         NAME
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      INTEGER               TABVAL     ( LBCELL:* )
      INTEGER               N
      INTEGER               VALUES     (        * )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol whose components are to be
!                    returned.
!     TABSYM,
!     TABPTR,
!     TABVAL     I   Components of the symbol table.
!
!     N          O   Dimension of the symbol.
!     VALUES     O   Values associated with the symbol.
!     FOUND      O   .TRUE. if the symbol NAME is in the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose components are to be
!              returned. If NAME is not in the symbol table, FOUND is
!              .FALSE.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of an integer symbol table.
!              The symbol NAME may or may not be in the symbol
!              table. The symbol table is not modified by this
!              subroutine.
!
! $ Detailed_Output
!
!     N        is the dimension of the symbol NAME. The dimension is
!              the number of values associated with the given symbol.
!              N is defined only if the output argument FOUND is
!              .TRUE.
!
!     VALUES   is an array containing the values associated with the
!              symbol. If the array is not large enough to hold all
!              of the values associated with NAME, as many as will
!              fit are returned. VALUES is defined only if the
!              output argument FOUND is .TRUE.
!
!     FOUND    is .TRUE. if NAME is in the symbol table.
!              If NAME is not in the table, FOUND is .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If there is an issue while reading the components of a
!         integer symbol table, an error is signaled by a routine in
!         the call tree of this routine. This normally indicates that
!         the integer symbol table is corrupted.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!         books   -->   5
!         erasers -->   6
!         pencils -->  12
!                      24
!         pens    -->  10
!                      12
!                      24
!
!      Let the dimension of VALUES be 3.
!
!      The calls,
!
!      CALL SYGETI ( 'pencils', TABSYM, TABPTR, TABVAL,
!     .               N,         VALUES, FOUND           )
!
!      CALL SYGETI ( 'pens',     TABSYM, TABPTR, TABVAL,
!     .               N,         VALUES, FOUND           )
!
!      CALL SYGETI ( 'desks',    TABSYM, TABPTR, TABVAL,
!     .               N,         VALUES, FOUND           )
!
!
!      return the values for N, VALUES, and FOUND associated with NAME:
!
!         NAME         N      VALUES    FOUND
!         ----------  ---    --------  -------
!         pencils      2        12      .TRUE.
!                               24
!         pens         3        10      .TRUE.
!                               12
!                               24
!         desks                        .FALSE.
!
! $ Restrictions
!
!     1)  This subroutine does not check to see if the output array
!         VALUES is large enough to hold all of the values associated
!         with the symbol NAME. The caller must provide the required
!         space.
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
!-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Moved entry
!        from $Exceptions to $Restrictions and added entry #1 in
!        $Exceptions.
!
!-    SPICELIB Version 1.0.2, 03-NOV-2005 (NJB)
!
!        Various header corrections were made. In particular,
!        the header no longer asserts that this routine will
!        "return as many values as will fit" in the output array
!        VALUES.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
!
!-&


! $ Index_Entries
!
!     fetch all components for a symbol
!
!-&


!
!     SPICELIB functions
!
      INTEGER               BSRCHC
      INTEGER               CARDC
      LOGICAL               RETURN
      INTEGER               SUMAI

!
!     Local variables
!
      INTEGER               NSYM

      INTEGER               LOCSYM
      INTEGER               LOCVAL



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYGETI' )
      END IF


!
!     How many symbols to start with?
!
      NSYM = CARDC ( TABSYM )

!
!     Is this symbol even in the table?
!
      LOCSYM = BSRCHC ( NAME, NSYM, TABSYM(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      IF ( LOCSYM .EQ. 0 ) THEN

         FOUND = .FALSE.

!
!     Otherwise, we can proceed without fear of error. Merely locate
!     and return the appropriate component from the values table.
!     We trust that the user has supplied enough room.
!
      ELSE

         FOUND = .TRUE.

         LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1
         N      = TABPTR(LOCSYM)

         CALL MOVEI ( TABVAL(LOCVAL), N, VALUES )

      END IF


      CALL CHKOUT ( 'SYGETI' )
      RETURN
      END
