!$Procedure DASUDC ( DAS, update data, character )

      subroutine dasudc ( handle, first, last, bpos, epos, data )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Update character data in a specified range of DAS logical
!     addresses with substrings of a character array.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations

      implicit none

      integer(ip)           handle
      integer(ip)           first
      integer(ip)           last
      integer(ip)           bpos
      integer(ip)           epos
      character(*)          data   ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DAS file handle.
!     FIRST,
!     LAST       I   Range of DAS character logical addresses.
!     BPOS,
!     EPOS       I   Begin and end positions of substrings.
!     DATA       I   Data having addresses FIRST through LAST.
!
! $ Detailed_Input
!
!     HANDLE   is a file handle of a DAS file opened for writing.
!
!     FIRST,
!     LAST     are the first and last of a range of DAS logical
!              addresses of characters. These addresses satisfy
!              the inequality
!
!                 1  <=   FIRST   <=   LAST   <=   LASTC
!
!              where LASTC is the last character logical address
!              in use in the DAS file designated by HANDLE.
!
!     BPOS,
!     EPOS     are the begin and end character positions that define the
!              substrings in each of the elements of the input array
!              that are to replace the data in the range of DAS
!              character addresses given by FIRST and LAST.
!
!     DATA     is an array of strings. The contents of the specified
!              substrings of the elements of the array DATA will be
!              written to the indicated DAS file in order:
!              DATA(1)(BPOS:BPOS) will be written to character logical
!              address FIRST; DATA(1)(BPOS+1:BPOS+1) will be written to
!              the character logical address FIRST+1, and so on; in this
!              ordering scheme, character (BPOS:BPOS) of DATA(I+1) is
!              the successor of character (EPOS:EPOS) of DATA(I).
!
!              DATA must be declared at least as
!
!                 CHARACTER*(EPOS)      DATA   ( R )
!
!              with the dimension R being at least
!
!                 R = INT( ( LAST - FIRST + SUBLEN ) / SUBLEN )
!
!              and SUBLEN, the length of each of the substrings in
!              the array to be written to the DAS file, being
!
!                 SUBLEN  =  EPOS - BPOS + 1
!
! $ Detailed_Output
!
!     None.
!
!     See $Particulars for a description of the effect of this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input file handle is invalid, an error is signaled by
!         a routine in the call tree of this routine.
!
!     2)  Only logical addresses that already contain data may be
!         updated: if either FIRST or LAST are outside the range
!
!            [ 1,  LASTC ]
!
!         where LASTC is the last character logical address that
!         currently contains data in the indicated DAS file, the error
!         SPICE(INVALIDADDRESS) is signaled. The DAS file will not be
!         modified.
!
!     3)  If EPOS or BPOS are outside of the range
!
!            [  1,  LEN( DATA(1) )  ]
!
!         the error SPICE(INVALIDINDEX) is signaled.
!
!     4)  If BPOS is greater than EPOS, the error
!         SPICE(INDICESOUTOFORDER) is signaled.
!
!     5)  If FIRST > LAST but both addresses are valid, this routine
!         will not modify the indicated DAS file. No error will be
!         signaled.
!
!     6)  If an I/O error occurs during the data update attempted
!         by this routine, the error is signaled by a routine in the
!         call tree of this routine. FIRST and LAST will not be
!         modified.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     DAS is a low-level format meant to store and transmit data. As
!     such, character data in DAS files are not interpreted by SPICELIB
!     DAS input or output routines. There are no limits on which
!     character values may be placed in the virtual character array of a
!     DAS file.
!
!     This routine replaces the character data in the specified range
!     of logical addresses within a DAS file with the contents of the
!     specified substrings of the input array DATA.
!
!     The actual physical write operations that update the indicated
!     DAS file with the contents of the input array DATA may not take
!     place before this routine returns, since the DAS system buffers
!     data that are written as well as data that are read. In any case,
!     the data will be flushed to the file at the time the file is
!     closed, if not earlier. A physical write of all buffered
!     records can be forced by calling the SPICELIB routine DASWBR
!     (DAS, write buffered records).
!
!     In order to append character data to a DAS file, filling in a
!     range of character logical addresses that starts immediately
!     after the last character logical address currently in use, the
!     SPICELIB routine DASADC (DAS add data, character) should be
!     used.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) The following example demonstrates the capabilities of the
!        DAS character data routines. The reader should notice that
!        in these interfaces, the character data are treated not as
!        strings (or arrays of strings) but as a stream of single
!        characters: DAS character data are not limited to
!        human-readable text. For example, one can store images or
!        DEM data as DAS character data.
!
!        The example shows how to add a variable amount of character
!        data to a new DAS file, how to update some of the character
!        logical addresses within that file, and how to read that
!        data out to a different array.
!
!        Example code begins here.
!
!              PROGRAM DASUDC_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         FNAME
!              PARAMETER           ( FNAME = 'dasudc_ex1.das' )
!
!              CHARACTER*(*)         TYPE
!              PARAMETER           ( TYPE  = 'TEST'           )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(22)        CDATIN ( 3  )
!              CHARACTER*(30)        CDATOU ( 10 )
!
!              INTEGER               HANDLE
!              INTEGER               I
!
!              DATA CDATOU  / '..............................',
!             .               '..............................',
!             .               '..............................',
!             .               '..............................',
!             .               '..............................',
!             .               '..............................',
!             .               '..............................',
!             .               '..............................',
!             .               '         1         2         3',
!             .               '123456789012345678901234567890' /
!
!        C
!        C     Open a new DAS file. Use the file name as the internal
!        C     file name, and reserve no records for comments.
!        C
!              CALL DASONW ( FNAME, TYPE, FNAME, 0, HANDLE )
!
!        C
!        C     Set the input data. Note that these data will be
!        C     considered as a binary data stream: DAS character data
!        C     are not limited to human-readable text. For example,
!        C     one can store images or DEM data as DAS character data.
!        C
!              CDATIN ( 1 ) = '--F-345678901234567890'
!              CDATIN ( 2 ) = '--S-345678901234567890'
!              CDATIN ( 3 ) = '--T-IRDxxxxxxxxxxxxxxx'
!
!        C
!        C     Add the last 20 characters of the first two elements
!        C     of CDATIN, and the 3rd character from the third one.
!        C
!              CALL DASADC ( HANDLE, 41, 3, 22, CDATIN )
!
!        C
!        C     Update the 10th, 20th and 30th character in the DAS
!        C     file with a vertical bar.
!        C
!              DO I = 1, 3
!
!                 CALL DASUDC ( HANDLE, I*10, I*10, 1, 1, '|' )
!
!              END DO
!
!        C
!        C     Close the file.
!        C
!              CALL DASCLS ( HANDLE )
!
!        C
!        C     Now verify the addition of data by opening the
!        C     file for read access and retrieving the data.
!        C
!              CALL DASOPR ( FNAME, HANDLE )
!
!        C
!        C     Read the 41 characters that we stored on the DAS
!        C     file. Update the data on the CDATOU array, placing
!        C     6 characters on each element, starting from the
!        C     10th position.
!        C
!              CALL DASRDC ( HANDLE, 1, 41, 10, 15, CDATOU )
!
!        C
!        C     Dump the data to the screen. Note that the last
!        C     three lines should remain unmodified, and that
!        C     only 5 characters will be written on the 7th line.
!        C
!              WRITE (*,*)
!              WRITE (*,*) 'Data from "', FNAME, '":'
!              WRITE (*,*)
!
!              DO I = 1, 10
!                 WRITE (*,*) CDATOU(I)
!              END DO
!
!        C
!        C     Close the file.
!        C
!              CALL DASCLS ( HANDLE )
!
!              END
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!         Data from "dasudc_ex1.das":
!
!         .........F-3456...............
!         .........789|12...............
!         .........345678...............
!         .........9|S-34...............
!         .........56789|...............
!         .........123456...............
!         .........7890T................
!         ..............................
!                  1         2         3
!         123456789012345678901234567890
!
!        Note that after run completion, a new DAS file exists in the
!        output directory.
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
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 19-MAY-2021 (NJB) (JDR)
!
!        Added error checks for invalid begin and end indices BPOS
!        and EPOS.
!
!        Added IMPLICIT NONE statement.
!
!        Updated entries in $Exceptions and $Revisions sections and
!        removed reference to nonexistent API from $Particulars.
!
!        Edited the header to comply with NAIF standard.
!
!        Replaced example code with one that demonstrates the usage and
!        effect of all DAS character data routines.
!
!        Updated entries in $Revisions section.
!
!-    SPICELIB Version 1.3.0, 10-APR-2014 (NJB)
!
!        Deleted declarations of unused parameters.
!
!        Corrected header comments: routine that flushes
!        written, buffered records is DASWBR, not DASWUR.
!
!-    SPICELIB Version 1.2.1, 19-DEC-1995 (NJB)
!
!        Corrected title of permuted index entry section.
!
!-    SPICELIB Version 1.2.0, 12-MAY-1995 (NJB)
!
!        Bug fix: routine handled values of BPOS incorrectly when
!        BPOS > 1.
!
!-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
!
!        Test of FAILED() added to loop termination conditions.
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!        Modified the $Examples section to demonstrate the new ID word
!        format which includes a file type and to include a call to the
!        new routine DASONW, open new for write, which makes use of the
!        file type. Also,  a variable for the type of the file to be
!        created was added.
!
!-    SPICELIB Version 1.0.0, 12-NOV-1992 (NJB) (WLT)
!
!-&

! $ Index_Entries
!
!     update a range of DAS logical addresses using substrings
!     write substrings to a range of DAS logical addresses
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.2.0, 12-MAY-1995 (NJB)
!
!        Bug fix: routine handled values of BPOS incorrectly when
!        BPOS > 1. This was due to the incorrect initialization
!        of the internal variables CHR and ELT. The initialization
!        was corrected.
!
!-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
!
!        Tests of FAILED() added to loop termination conditions.
!        Without these tests, infinite loops could result if DASA2L or
!        DASURC signaled an error inside the loops.
!
!-&

!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip), parameter :: nwc = 1024

      integer(ip), parameter :: char = 1

!
!     Local variables
!
      integer(ip)           chr
      integer(ip)           clbase
      integer(ip)           clsize
      integer(ip)           elt
      integer(ip)           l
      integer(ip)           lastc
      integer(ip)           lastd
      integer(ip)           lasti
      integer(ip)           n
      integer(ip)           nmove
      integer(ip)           nmoved
      integer(ip)           numchr
      integer(ip)           nwritn
      integer(ip)           recno
      integer(ip)           rcpos
      integer(ip)           wordno

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Get the last logical addresses in use in this DAS file.
!
      call daslla ( handle, lastc, lastd, lasti )

!
!     Validate the input addresses.
!
      if (      ( first < 1     ) &
           .or. ( first > lastc ) &
           .or. ( last  < 1     ) &
           .or. ( last  > lastc )  ) then

         call setmsg ( 'FIRST was #. LAST was #. Valid range is [1,#].')
         call errint ( '#',   first                                    )
         call errint ( '#',   last                                     )
         call errint ( '#',   lastc                                    )
         call sigerr ( 'SPICE(INVALIDADDRESS)'                         )
         return

      end if

!
!     Make sure BPOS and EPOS are valid and compatible with the string
!     length of the input array.
!
      if (  ( bpos < 1 ) .or. ( bpos > len( data(1) ) )  ) then

         call setmsg ( 'String begin index must be ' &
         //            'in the range #:# but was #.' )
         call errint ( '#', 1                        )
         call errint ( '#', len( data(1) )           )
         call errint ( '#', bpos                     )
         call sigerr ( 'SPICE(INVALIDINDEX)'         )
         return

      end if

      if (  ( epos < 1 ) .or. ( epos > len( data(1) ) )  ) then

         call setmsg ( 'String end index must be in ' &
         //            'the range #:# but was #.'    )
         call errint ( '#', 1                        )
         call errint ( '#', len( data(1) )           )
         call errint ( '#', epos                     )
         call sigerr ( 'SPICE(INVALIDINDEX)'         )
         return

      end if

      if ( bpos > epos ) then

         call setmsg ( 'String begin index # must be less ' &
         //            'than or equal to the end index #.' )
         call errint ( '#', bpos                           )
         call errint ( '#', epos                           )
         call sigerr ( 'SPICE(INDICESOUTOFORDER)'          )
         return

      end if
!
!     Get the length of the substrings of DATA.  Count the total number
!     of characters to write.
!
      l       =  epos - bpos  + 1
      n       =  last - first + 1
      nwritn  =  0

!
!     Find out the physical location of the first character to update.
!
      call dasa2l ( handle, char, first, clbase, clsize, recno, wordno )

!
!     Write as much data into record RECNO as is necessary and possible.
!
!     NUMCHR is the number of characters to write to the current record.
!
!     ELT is the index of the element of the input array that we're
!     taking data from.  CHR is the position in that array element of
!     the next character to move to the file.
!
!     NMOVED is the number of characters we've moved into the current
!     record so far.
!
!     RCPOS is the character position we'll write to next in the current
!     record.
!
      numchr  =  min ( n,  nwc - wordno + 1 )
      elt     =  1
      chr     =  bpos
      nmoved  =  0
      rcpos   =  wordno

      do while (  ( nmoved < numchr ) .and. ( .not. failed() )  )

         if ( chr > epos ) then
            elt = elt + 1
            chr = bpos
         end if

!
!        Find out how many characters to move from the current array
!        element to the current record.
!
         nmove   =  min (  numchr - nmoved,   epos - chr + 1  )

!
!        Update the current record.
!
         call dasurc (  handle, &
                        recno, &
                        rcpos, &
                        rcpos + nmove - 1, &
                        data(elt) ( chr  :  chr + nmove - 1 )   )

         nmoved  =  nmoved + nmove
         rcpos   =  rcpos  + nmove
         chr     =  chr    + nmove

      end do

      nwritn =  numchr
      recno  =  recno + 1

!
!     Update as many additional records as necessary.
!

      do while (  ( nwritn < n ) .and. ( .not. failed() )  )

!
!        At this point, RECNO is the correct number of the record to
!        write to next.  CLBASE is the number of the first record of
!        the cluster we're about to write to.
!
         if (  recno  <  ( clbase + clsize )  ) then

!
!           We can continue writing the current cluster.  Find
!           out how many elements to write to the current record,
!           and write them.
!
            numchr  =  min ( n - nwritn,  nwc )
            nmoved  =  0
            rcpos   =  1

            do while ( ( nmoved < numchr ) .and. ( .not. failed() ) )

               if ( chr > l ) then
                  elt = elt + 1
                  chr = bpos
               end if

!
!              Find out how many characters to move from the array
!              element to the current record.
!
               nmove   =  min (  numchr - nmoved,   epos - chr + 1  )

               call dasurc (  handle, &
                              recno, &
                              rcpos, &
                              rcpos + nmove - 1, &
                              data(elt) ( chr  :  chr + nmove - 1 )  )

               nmoved  =  nmoved + nmove
               rcpos   =  rcpos  + nmove
               chr     =  chr    + nmove

            end do

            nwritn  =   nwritn + numchr
            recno   =   recno  + 1

         else

!
!           We must find the next character cluster to write to.
!           The first character in this cluster has address FIRST +
!           NWRITN.
!
            call dasa2l ( handle, &
                          char, &
                          first + nwritn, &
                          clbase, &
                          clsize, &
                          recno, &
                          wordno  )

         end if

      end do

      return
      end
