!$Procedure EXCESS ( Report an excess of elements in a cell )

      subroutine excess ( number, struct )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Set the long error message so as to indicate the number of excess
!     elements encountered by a routine operating on cells or on data
!     structures based on cells.
!
! $ Required_Reading
!
!     CELLS
!
! $ Keywords
!
!     CELLS
!     ERROR
!
! $ Declarations

      implicit none

      integer(ip)      number
      character(*)     struct

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NUMBER     I   Number of excess elements.
!     STRUCT     I   Name of the data structure.
!
! $ Detailed_Input
!
!     NUMBER   is the number of excess elements encountered.
!              This may be zero or negative, which indicates
!              no excess.
!
!     STRUCT   is the name of the data structure being manipulated.
!              Typically, this is one of the strings: 'cell', 'set',
!              or 'symbol table'. However, it may be any character
!              string. STRUCT should NOT end in a period.
!              The period at the end of the message is supplied
!              automatically.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not detect any errors.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is part of the SPICELIB error handling mechanism.
!
!     EXCESS sets the long error message. The message has the form:
!
!                 An excess of <NUMBER> element(s) could
!                 not be accommodated in the output <STRUCT>.
!
!     Leading and trailing blanks in STRUCT are removed. If there is
!     no excess (NUMBER is zero or negative), then is blank.
!
! $ Examples
!
!     The response of EXCESS to a variety of inputs is illustrated
!     below.
!
!           NUMBER = 1
!           STRUCT = 'set'
!           ERROR  = 'An excess of 1 element could not
!                     be accommodated in the output set.'
!
!           NUMBER = 5
!           STRUCT = 'stack'
!           ERROR  =  An excess of 5 elements could not
!                     be accommodated in the output stack.'
!
!           NUMBER = 0
!           STRUCT =
!           ERROR  = ' '
!
!           NUMBER = -6
!           STRUCT =
!           ERROR  = ' '
!
!     In particular, note that EXCESS does not set the long error
!     message when the number of excess elements is not positive. Also,
!     the singular 'element' is used for an excess of one, while
!     the plural 'elements' is used for all other positive excesses.
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
!     C.A. Curzon        (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
!
!-&


! $ Index_Entries
!
!     report an excess of elements in a cell
!
!-&


! $ Revisions
!
!-    Beta Version 2.0.0, 11-JAN-1989 (NJB)
!
!        Sets the long error message directly. No longer returns
!        an error message. Message no longer contains name of
!        routine which detected the error.
!
!-&


!
!     SPICELIB functions
!
      logical               return


!
!     Local variables
!
      integer(ip)           longln
      parameter           ( longln = 320 )
      character(longln)     error
!
!     Set up the error processing.
!
      if ( return () ) return
!
!     If there is no excess, don't report one.
!
      if ( number > 0 ) then

!
!        Begin with the number. We will build the rest of the
!        message around it.
!
         call intstr ( number, error )

!
!        A short blurb goes in front of the number.
!
         call prefix ( 'An excess of', 1, error )

!
!        Singular or plural?
!
         if ( number == 1 ) then
            call suffix ( 'element', 1, error )
         else
            call suffix ( 'elements', 1, error )
         end if

!
!        Another short blurb.
!
         call suffix ( 'could not be accommodated in the output', &
                       1, &
                       error                                      )

!
!        And the name of the structure.
!
         call suffix ( struct, 1, error   )

!
!        And a period at the end, to complete the sentence.
!
         call suffix ( '.', 0, error )

!
!        Set the long error message:
!
         call setmsg ( error )

      else

          error = ' '

      end if


      return
      end
