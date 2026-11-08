!$Procedure     QTRAN

      SUBROUTINE QTRAN ( INPUT, OUTPUT, TRAN )

! $ Abstract
!
!     Prompt the user to supply values for the first query in a string.
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
! $ Keywords
!
!     PARSE, QUERY
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         INPUT
      CHARACTER*(*)         OUTPUT
      LOGICAL               TRAN


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INPUT      I   Input string, possibly containing queries.
!     OUTPUT     O   Equivalent string, with first query replaced.
!     TRAN       O   True when a query was replaced.
!
! $ Detailed_Input
!
!     INPUT      is the input string. This may contain any number
!                of queries, for which the user will be expected to
!                supply values. A query is any string of up to 32
!                consecutive non-blank characters ending with '?'.
!
! $ Detailed_Output
!
!     OUTPUT     is the equivalent of INPUT after the first of the
!                queries in INPUT has been supplied with a value.
!
!                OUTPUT may overwrite INPUT.
!
!      TRAN      is true whenever a query was found and replaced, and is
!                false otherwise.
!
! $ Input_Files
!
!     None.
!
! $ Output_Files
!
!     None.
!
! $ Input_Output_Common
!
!     None.
!
! $ Exceptions
!
!     It is possible that query resolution will result in an overflow
!     of the output string.  This situation is dianosed by a routine
!     called by QTRAN.
!
! $ Detailed_Description
!
!     Look for a query in INPUT. (It will end with '?'.) Ask the user
!     to supply a value for the query. Insert the value into OUTPUT in
!     place of the query itself.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     I. M. Underwood (JPL)
!
! $ Version_and_Date
!
!     Version 1.1, 14-SEP-1995
!
!        Assignment to otherwise unused variable SUPRES deleted.
!
!     Version 1,   17-SEP-1986
!
!-&



!
!     OPTLIB functions
!
      INTEGER               LASTNB

!
!     Local variables
!
      CHARACTER*(1)         EQUOTE
      CHARACTER*(1)         DELIM
      CHARACTER*33          QUERY
      INTEGER               LOC
      INTEGER               QLEN
      CHARACTER*128         REPLY
      INTEGER               RLEN

      CHARACTER*55          PROMPT
      INTEGER               I

!
!     Look up the special marker used for suppressing query
!     evaluation.
!
      CALL GETEQ ( EQUOTE )
      CALL GETDEL( DELIM  )


!
!     Look at each word. If a word ends with '?', it's a query.
!     (QUERY is a character longer than a valid query. So any
!     valid query will have at least one blank at the end.)
!
      TRAN   = .FALSE.
      I      =  1



      CALL NTHUQW ( INPUT, I, EQUOTE, QUERY, LOC )

      DO WHILE ( (.NOT. TRAN)  .AND.  QUERY .NE. ' ' )

!
!        First we have to look for the translation supression
!        character.
!



         TRAN = (       ( INDEX (QUERY,'? ') .GT.  0       ) &
                  .AND. (        QUERY       .NE. '?'     ) )

         IF ( .NOT. TRAN ) THEN
            I = I + 1
            CALL NTHUQW ( INPUT, I, EQUOTE, QUERY, LOC )
         END IF


      END DO


      OUTPUT = INPUT

!
!     If we found a query, get the user's response, and insert it
!     in place of the query.
!
      IF ( TRAN ) THEN

         QLEN   = LASTNB ( QUERY )
         PROMPT = 'Enter value for ' // QUERY(1:QLEN-1) // ' > '

         CALL RDSTMN  ( PROMPT, DELIM, REPLY   )
         RLEN   = MAX ( 1,  LASTNB ( REPLY ) )

         CALL REPSUB  ( OUTPUT, LOC, LOC+QLEN-1, REPLY(1:RLEN), OUTPUT )

       END IF

      RETURN
      END
