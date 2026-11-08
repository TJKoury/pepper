!$Procedure WNSUMD ( Summary of a double precision window )

      subroutine wnsumd ( window, &
                          meas,   avg,   stddev, &
                          idxsml, idxlon         )

! $ Abstract
!
!     Summarize the contents of a double precision window.
!
! $ Required_Reading
!
!     WINDOWS
!
! $ Keywords
!
!     WINDOWS
!
! $ Declarations

      implicit none

      integer               lbcell
      parameter           ( lbcell = -5 )

      double precision      window    ( lbcell:* )
      double precision      meas
      double precision      avg
      double precision      stddev
      integer               idxsml
      integer               idxlon

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     WINDOW     I   Window to be summarized.
!     MEAS       O   Total measure of intervals in WINDOW.
!     AVG        O   Average measure.
!     STDDEV     O   Standard deviation.
!     IDXSML,
!     IDXLON     O   Locations of shortest, longest intervals.
!
! $ Detailed_Input
!
!     WINDOW   is a window containing zero or more intervals.
!
! $ Detailed_Output
!
!     MEAS     is the total measure of the intervals in the input
!              window. This is just the sum of the measures of the
!              individual intervals.
!
!     AVG      is the average of the measures of the intervals in the
!              input window.
!
!     STDDEV   is the standard deviation of the measures of the
!              intervals in the input window.
!
!     IDXSML,
!     IDXLON   are the locations of the shortest and longest intervals
!              in the input window. The shortest interval is
!
!                 [ WINDOW(IDXSML), WINDOW(IDXSML+1) ]
!
!              and the longest is
!
!                 [ WINDOW(IDXLON), WINDOW(IDXLON+1) ]
!
!              IDXSML and IDXLON are both zero if the input window
!              contains no intervals.
!
!              If WINDOW contains multiple intervals having the shortest
!              length, IDXSML is the index of the first such interval.
!              Likewise for the longest length.
!
!              Indices range from 1 to 2*N-1, where N is the number of
!              intervals in the window.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If WINDOW has odd cardinality, the error
!         SPICE(INVALIDCARDINALITY) is signaled.
!
!     2)  Left endpoints of stored intervals must be strictly greater
!         than preceding right endpoints. Right endpoints must be
!         greater than or equal to corresponding left endpoints.
!         Invalid window data are not diagnosed by this routine and may
!         lead to unpredictable results.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides a summary of the input window, consisting
!     of the following items:
!
!     -  The measure of the window.
!
!     -  The average and standard deviation of the measures
!        of the individual intervals in the window.
!
!     -  The indices of the left endpoints of the shortest
!        and longest intervals in the window.
!
!     All of these quantities are zero if the window contains no
!     intervals.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as input,
!     the compiler and supporting libraries, and the machine specific
!     arithmetic implementation.
!
!     1) Define a window with six intervals, and calculate the
!        summary for that window.
!
!
!        Example code begins here.
!
!
!              PROGRAM WNSUMD_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               WNCARD
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         FMT1
!              PARAMETER           ( FMT1   = '(A,F11.6)' )
!
!              CHARACTER*(*)         FMT2
!              PARAMETER           ( FMT2   = '(A,I4)' )
!
!              CHARACTER*(*)         FMT3
!              PARAMETER           ( FMT3   = '(A,2(F6.3,A))' )
!
!              INTEGER               LBCELL
!              PARAMETER           ( LBCELL = -5 )
!
!              INTEGER               WNSIZE
!              PARAMETER           ( WNSIZE = 12 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      AVG
!              DOUBLE PRECISION      LEFT
!              DOUBLE PRECISION      MEAS
!              DOUBLE PRECISION      RIGHT
!              DOUBLE PRECISION      STDDEV
!              DOUBLE PRECISION      WINDOW ( LBCELL:WNSIZE )
!
!              INTEGER               IDXLON
!              INTEGER               IDXSML
!              INTEGER               INTLON
!              INTEGER               INTSML
!
!        C
!        C     Validate the WINDOW with size WNSIZE and zero elements.
!        C
!              CALL WNVALD ( WNSIZE, 0, WINDOW )
!
!        C
!        C     Insert the intervals
!        C
!        C        [  1,  3 ] [  7, 11 ] [ 18, 18 ]
!        C        [ 23, 27 ] [ 30, 69 ] [ 72, 80 ]
!        C
!        C     into WINDOW.
!        C
!              CALL WNINSD (  1.0D0,  3.0D0, WINDOW )
!              CALL WNINSD (  7.0D0, 11.0D0, WINDOW )
!              CALL WNINSD ( 18.0D0, 18.0D0, WINDOW )
!              CALL WNINSD ( 23.0D0, 27.0D0, WINDOW )
!              CALL WNINSD ( 30.0D0, 69.0D0, WINDOW )
!              CALL WNINSD ( 72.0D0, 80.0D0, WINDOW )
!
!        C
!        C     Calculate the summary for WINDOW.
!        C
!              CALL WNSUMD ( WINDOW, MEAS,   AVG,
!             .              STDDEV, IDXSML, IDXLON )
!
!        C
!        C     IDXSML and IDXLON refer to the indices of
!        C     the SPICE Cell data array.
!        C
!              INTSML = (IDXSML+1)/2
!              INTLON = (IDXLON+1)/2
!
!              WRITE(*,FMT1) 'Measure           : ', MEAS
!              WRITE(*,FMT1) 'Average           : ', AVG
!              WRITE(*,FMT1) 'Standard Dev      : ', STDDEV
!              WRITE(*,FMT2) 'Index shortest    : ', IDXSML
!              WRITE(*,FMT2) 'Index longest     : ', IDXLON
!              WRITE(*,FMT2) 'Interval shortest : ', INTSML
!              WRITE(*,FMT2) 'Interval longest  : ', INTLON
!
!        C
!        C     Output the shortest and longest intervals.
!        C
!              CALL WNFETD ( WINDOW, INTSML, LEFT, RIGHT )
!              WRITE(*,FMT3) 'Shortest interval : [ ', LEFT, ', ',
!             .                                        RIGHT, ' ]'
!              CALL WNFETD ( WINDOW, INTLON, LEFT, RIGHT )
!              WRITE(*,FMT3) 'Longest interval  : [ ', LEFT, ', ',
!             .                                        RIGHT, ' ]'
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Measure           :   57.000000
!        Average           :    9.500000
!        Standard Dev      :   13.413302
!        Index shortest    :    5
!        Index longest     :    9
!        Interval shortest :    3
!        Interval longest  :    5
!        Shortest interval : [ 18.000, 18.000 ]
!        Longest interval  : [ 30.000, 69.000 ]
!
!
!     2) Let A contain the intervals
!
!           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
!
!        Let B contain the singleton intervals
!
!           [ 2, 2 ]  [ 9, 9 ]  [ 27, 27 ]
!
!        The measures of A and B are
!
!           (3-1) + (11-7) + (27-23) = 10
!
!        and
!
!           (2-2) + (9-9) + (27-27) = 0
!
!        respectively. Each window has three intervals; thus, the
!        average measures of the windows are 10/3 and 0. The standard
!        deviations are
!
!             .-----------------------------------------
!             |       2         2          2
!             |  (3-1)  + (11-7)  + (27-23)           2           1/2
!             |  ---------------------------  - (10/3)     = (8/9)
!             |             3
!           \ |
!            \|
!
!        and 0. Neither window has one "shortest" interval or "longest"
!        interval; so the first ones found are returned: IDXSML and
!        IDXLON are 1 and 3 for A, 1 and 1 for B.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 05-JUL-2021 (JDR) (NJB)
!
!        Changed output argument names SMALL and LONG to IDXSML and
!        IDXLON for consistency with other routines.
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example. Added entry #2 in $Exceptions section.
!
!        Improved description of arguments IDXSML and IDXLON in
!        $Detailed_Output.
!
!        Removed unnecessary $Revisions section.
!
!-    SPICELIB Version 1.1.0, 25-FEB-2009 (EDW)
!
!        Added error test to confirm input window has even cardinality.
!        Corrected section order to match NAIF standard.
!
!-    SPICELIB Version 1.0.2, 29-JUL-2002 (NJB)
!
!        Corrected error in example section: changed claimed value
!        of longest interval for window A from 2 to 3.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     summary of a d.p. window
!
!-&


!
!     SPICELIB functions
!
      integer               cardd
      logical               return
      logical               even


!
!     Local variables
!
      double precision      m
      double precision      sum
      double precision      sumsqr
      double precision      mshort
      double precision      mlong

      integer               i
      integer               card


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

!
!     Get the cardinality (number of endpoints) of the window.
!
      card = cardd ( window )

!
!     Confirm evenness of CARD.
!
      if ( .not. even( card ) ) then

         call chkin  ( 'WNSUMD' )
         call setmsg ( 'Input window has odd cardinality. A valid ' &
                  //   'SPICE window must have even element ' &
                  //   'cardinality.' )
         call sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         call chkout ( 'WNSUMD' )
         return

      end if

!
!     Trivial case: no intervals. Return all zeros.
!
      if ( card .eq. 0 ) then

         meas   = 0.d0
         avg    = 0.d0
         stddev = 0.d0
         idxsml  = 0
         idxlon   = 0

!
!     Collect the sum of the measures and the squares of the measures
!     for each of the intervals in the window. At the same time, keep
!     track of the shortest and longest intervals encountered.
!
      else

         sum    = 0.d0
         sumsqr = 0.d0

         idxsml  = 1
         mshort = window(2) - window(1)

         idxlon   = 1
         mlong  = window(2) - window(1)

         do i = 1, card, 2

            m       = window(i+1) - window(i)
            sum     = sum         + m
            sumsqr  = sumsqr      + m*m

            if ( m .lt. mshort ) then
               idxsml  = i
               mshort = m
            end if

            if ( m .gt. mlong ) then
               idxlon  = i
               mlong = m
            end if

         end do

!
!        The envelope please?
!
         meas   = sum
         avg    = meas * 2.d0 / dble ( card )
         stddev = dsqrt ( ( sumsqr * 2.d0 / dble ( card ) ) - avg*avg )

      end if

      return
      end
