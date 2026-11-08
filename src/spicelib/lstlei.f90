!$Procedure LSTLEI ( Last integer element less than or equal to )

      INTEGER FUNCTION  LSTLEI ( X, N, ARRAY )

! $ Abstract
!
!     Find the index of the largest array element less than or equal
!     to a given integer X in an array of non-decreasing integers.
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
!     ARRAY
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INTEGER          X
      INTEGER          N
      INTEGER          ARRAY ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X          I   Upper bound value to search against.
!     N          I   Number of elements in ARRAY.
!     ARRAY      I   Array of possible lower bounds.
!
!     The function returns the index of the last element of ARRAY that
!     is less than or equal to X.
!
! $ Detailed_Input
!
!     X        is an integer value acting as an upper bound: the element
!              of ARRAY that is the greatest element less than or equal
!              to X is to be found.
!
!     N        is the total number of elements in ARRAY.
!
!     ARRAY    is an array of integers that forms a non-decreasing
!              sequence. The elements of array need not be distinct.
!
! $ Detailed_Output
!
!     The function returns the index of the highest-indexed element in
!     the input array that is less than or equal to X. The routine
!     assumes the array elements are sorted in non-decreasing order.
!
!     Indices range from 1 to N.
!
!     If all elements of ARRAY are greater than X, the routine returns
!     the value 0. If N is less than or equal to zero, the routine
!     returns the value 0.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If N is less than or equal to zero, the function returns 0.
!         This case is not treated as an error.
!
!     2)  If the input array is not sorted in non-decreasing order, the
!         output of this routine is undefined. No error is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine uses a binary search algorithm and so requires
!     at most on the order of
!
!        log (N)
!           2
!
!     steps to compute the value of LSTLEI.
!
!     Note: If you need to find the first element of the array that is
!     greater than X, simply add 1 to the result returned by this
!     function and check to see if the result is within the array bounds
!     given by N.
!
! $ Examples
!
!     Suppose that you have an reasonably large ordered array of
!     integers, into which you want to insert a few more without
!     destroying the ordering.
!
!     Depending upon your application, it may be desirable to
!     not insert duplicates, to insert duplicates before
!     existing entries or to insert them after existing entries.
!
!     The code fragment below, illustrates an insertion scheme
!     that will insert duplicate items after existing items
!     and simultaneously update a second parallel array of
!     double precision numbers.
!
!           get the pair to insert
!
!           READ (*,*) KEY, VALUE
!
!           locate the place to insert the new KEY into the sorted
!           array of keys.
!
!           LOC = LSTLEI ( KEY, NKEYS, KEYS ) + 1
!
!           insert the key and its associated value into the
!           KEYS and  VALUES arrays at location LOC
!
!           CALL INSLAI ( KEY,   1, LOC, NKEYS, KEYS   )
!           CALL INSLAD ( VALUE, 1, LOC, NVALS, VALUES )
!
!     If at the READ statement the arrays KEYS and VALUES looked like:
!
!           KEYS     VALUES     NKEYS = 6, NVALS = 6
!           ----     -------
!             2       3.00D0
!             5       1.00D0
!             7       3.14D0
!            16       7.11D0
!            18       2.14D0
!            23      12.12D0
!
!     and 9 and 33.33D3 were read into KEY and VALUE respectively
!     then LSTLEI (KEY, NKEYS, KEYS ) would be 3 and LOC would be 4.
!     After the calls to the routines INSLAI and INSLAD we would have
!
!           KEYS     VALUES     NKEYS = 7, NVALS = 7
!           ----     -------
!             2       3.00D0
!             5       1.00D0
!             7       3.14D0
!             9      33.33D3     <===== inserted items.
!            16       7.11D0
!            18       2.14D0
!            23      12.12D0
!
!     If 7 and 33.33D3 were read into KEY and VALUE respectively
!     then again LSTLEI (KEY, NKEYS, KEYS ) would be 3 and LOC would
!     be 4. After the calls to the routines INSLAI and INSLAD we
!     would have:
!
!           KEYS     VALUES     NKEYS = 7, NVALS = 7
!           ----     -------
!             2       3.00D0
!             5       1.00D0
!             7       3.14D0
!             7      33.33D3     <===== inserted items.
!            16       7.11D0
!            18       2.14D0
!            23      12.12D0
!
!     If we replaced the line of code
!
!           LOC = LSTLEI ( KEY, NKEYS, KEYS ) + 1
!     by
!
!           LOC = LSTLTI ( KEY, NKEYS, KEYS ) + 1
!
!     we would obtain a routine that inserted duplicates before
!     existing entries. (LSTLTI is similar to LSTLEI except it finds
!     the last occurrence of an integer strictly less than a value.)
!     Using 7 and 33.33D3 for KEY and VALUE again, the modified code
!     fragment would yield the results shown below.
!
!           KEYS     VALUES     NKEYS = 7, NVALS = 7
!           ----     -------
!             2       3.00D0
!             5       1.00D0
!             7      33.33D3     <===== inserted items.
!             7       3.14D0
!            16       7.11D0
!            18       2.14D0
!            23      12.12D0
!
!
!     (Note: you should NOT use the
!     code outlined above as the basis of a sorting algorithm.
!     The NAIF routines SHELLI, SHELLD, SHELLC, ORDERI, ORDERD, ORDERC,
!     REORDI, REORDD and REORDC are much more efficient routines for
!     sorting arrays or sorting a set of parallel arrays using
!     one of the set as a key. The fragment presented here is useful
!     for performing update insertions into previously ordered arrays.)
!
!     For more ideas regarding the use of this routine, see LSTLEC
!     and LSTLTC.
!
! $ Restrictions
!
!     1)  If the sequence of integer numbers in the input array ARRAY is
!         not non-decreasing, the program will run to completion but the
!         index found will not mean anything.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section. Improved $Detailed_Input,
!        $Detailed_Output, $Particulars, $Exceptions and $Restrictions
!        sections.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN)
!
!-&


! $ Index_Entries
!
!     last integer element less_than_or_equal_to
!
!-&


!
!     Local variables
!
      INTEGER          J

      INTEGER          BEGIN
      INTEGER          END
      INTEGER          MIDDLE
      INTEGER          ITEMS



      ITEMS = N

      BEGIN = 1
      END   = N

      IF      ( N .LE. 0            ) THEN

!
!        There's nobody home---that is there is nothing in the array
!        to compare against.  Zero is the only sensible thing to return.
!
         LSTLEI = 0

      ELSE IF ( X .LT. ARRAY(BEGIN) ) THEN

!
!        None of the array elements are less than or equal to X
!
         LSTLEI = 0

      ELSE IF ( X .GE. ARRAY(END)   ) THEN

!
!        X is greater than or equal to all elements of the array.  Thus
!        the last element of the array is the last item less than or
!        equal to X.
!
         LSTLEI = END

      ELSE

!
!        X lies between some pair of elements of the array
!

         DO WHILE ( ITEMS .GT. 2 )

            J        = ITEMS/2
            MIDDLE   = BEGIN + J

            IF ( ARRAY(MIDDLE) .LE. X ) THEN
               BEGIN = MIDDLE
            ELSE
               END   = MIDDLE
            END IF

            ITEMS    = 1 + (END - BEGIN)
         END DO

         LSTLEI = BEGIN

      END IF

      RETURN
      END
