!$Procedure ZZFTPSTR ( Private --- Fetch FTP Validation String )

      subroutine zzftpstr ( tstcom, lend, rend, delim )
      use pepper_kinds, only: ip

! $ Abstract
!
!    Retrieve the components of the FTP validation string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      implicit none

      include              'zzftprms.inc'

      character*(*)         tstcom
      character*(*)         lend
      character*(*)         rend
      character*(*)         delim

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TSTCOM     O   The FTP test component string.
!     LEND       O   String that brackets TSTCOM on the left in a file.
!     REND       O   String that brackets TSTCOM on the right in a file.
!     DELIM      O   Delimiter that separates the pieces of TSTCOM.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     TSTCOM     is a string composed of clusters of characters that
!                are susceptible to FTP ASCII mode transfer corruption,
!                separated by the DELIM character.  For example:
!
!                    :<CLUSTR(1)>:<CLUSTR(2):...
!                                            ...<CLUSTR(N)>:
!
!                where <CLUSTR(I)> is one cluster of characters that
!                is subject to improper FTP corruption. The string
!                that is to receive this value should be SIZSTR
!                characters in length.
!
!     LEND,      are the two sequences of printing characters that
!     REND       bracket TSTCOM in the binary file.  Their purpose is
!                to permit proper detection of TSTCOM in the event
!                of compression or expansion, due to improper FTP
!                transfer.  The variables which are to receive these
!                values should be SIZEND characters in length.
!
!     DELIM      is the printing character delimiter that separates the
!                test character clusters from one another, as well as
!                LEND and REND.  Since it is often the case that pairs
!                or triples of non-printing characters will trigger
!                FTP corruption, this delimiter blocks any unintended
!                interaction.
!
! $ Parameters
!
!     1) See include file zzftprms.inc
!
!     2) Since inserting non-printing characters into strings is a
!        somewhat arduous task requiring extensive use of the intrinsic
!        CHAR, integer parameters that map to the needed ASCII codes are
!        defined with variable names INT###, where ### is replaced with
!        the three digit ASCII integer code.  For each such integer
!        code, there is a corresponding character parameter whose name
!        is of the form ASC###.  For example:
!
!           INT010 = 10  -> ASC010 = <10> or <LF>
!           INT206 = 206 -> ASC206 = <206>
!
!        where <#> refers to CHAR(#) or CHAR(ICHAR('#')) in the case of
!        LF(line feed).
!
!        These naming conventions should be preserved when the FTP
!        validation string is updated.
!
! $ Files
!
!     While this routine is designed to aid in the detection of
!     improper FTP transfers, it simply returns the candidate
!     string for validation and does not interact with any
!     files directly.
!
! $ Exceptions
!
!     Error Free.
!
! $ Particulars
!
!     To minimize code alterations in the event of a string update,
!     the calling routine that declares the variables to receive
!     the strings stored here should include zzftprms.inc and utilize
!     the size parameters defined there as recommended in the Detailed
!     I/O sections above.
!
!     This private SPICELIB routine is designed to centralize the
!     definition of the FTP validation string present in binary
!     SPICE kernels.  If in the process of FTP'ing a binary
!     file from one platform to another, the user neglects to
!     invoke the IMAGE (BINARY) transfer mode, an ASCII mode
!     transfer may occur.  As this at the very least may substitute
!     one set of line terminators for another, corruption of the
!     binary file is likely.  By placing a string that encapsulates
!     a representative set of these character sequences that are
!     susceptible to corruption in the file record, it is possible
!     to trap and report any problems to the user when corrupted
!     kernels are loaded at run time.
!
!     To that end, analysis of evidence obtained by moving test binary
!     files from one platform to another indicates the following
!     clusters of ASCII codes are likely candidates for corruption:
!
!        Test Clusters:
!
!        <13>      - Text line terminator on Macintosh-based platforms.
!        <10>      - Text line terminator on UNIX-based platforms.
!        <13><10>  - Text line terminator on Microsoft platforms.
!        <13><0>   - Sequence of characters that maps into <13> on some
!                    UNIX-based systems. (HP, SGI, NEXT)
!        <129>     - Macintosh based systems permute ASCII values whose
!                    parity bit is set.  Codes in excess of ASCII
!                    127 are altered.
!        <16><206> - Some ancient FTP servers on PC's convert this
!                    sequence of ASCII characters to <16><16><206>.
!
!     The examples above show that substitution of one set of line
!     terminators for another can result in expansion or compression of
!     certain sequences of bytes.  If the clusters were juxtaposed, new
!     sequences of adjacent bytes, themselves subject to transformation,
!     might be formed.  So the FTP test string present in the binary
!     file should have some mechanism for preventing interaction between
!     the clusters.  The test string should also be constructed so that
!     it can be easily located in the event compression or expansion,
!     either internally or elsewhere in the file record, shifts it away
!     from its default location.
!
!     So by separating these clusters with a printable delimiter, then
!     bracketing the entire test string with start and stop identifiers,
!     we have a reasonable mechanism for locating and analyzing any
!     potential FTP corruption. Then the sequence of characters to be
!     inserted into the file will appear as:
!
!        FTPSTR:<13>:<10>:<13><10>:<13><0>:<129>:<16><206>:ENDFTP
!
!     where 'FTPSTR' and 'ENDFTP' are the bracketing substrings and
!     ':' is the delimiting character.
!
!     By no means do we claim that these are the complete set of
!     clusters that are corruptible through an improper FTP transfer.
!     An update procedure is provided in the Revisions section just
!     after the routine header.  Following this procedure will require
!     the least amount of effort to prevent older files from falsely
!     indicating corruption under new Toolkits, as well as newer files
!     failing on old Toolkits.
!
!
! $ Examples
!
!     This routine just fetches the components of the FTP validation
!     string.
!
! $ Restrictions
!
!     1) TSTCOM, LEND, REND, and DELIM must be large enough to hold
!        the entire values returned by this routine, otherwise
!        truncation will occur.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 21-MAR-1999 (FST)
!
!
!-&

! $ Index_Entries
!
!     fetch the ftp validation string components
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.0.0, 21-MAR-1999 (FST)
!
!        FTP validation string update procedure:
!
!           (1) Leave 'FTPSTR', 'ENDFTP', and ':' alone, as
!               their alteration will require special
!               consideration for older files.
!
!           (2) Leave the existing test clusters in the
!               existing order, and place any new clusters
!               between the last ':' and the E in 'ENDFTP'.
!               Make certain these are ':' delimited as well.
!
!           (3) Modify the contents of zzftprms.inc to
!               indicate the new sizes of the various string
!               components. Routines that include this must
!               then be recompiled.
!
!-&

!
!     Local Parameters
!
!     Maximum size of an individual test cluster component
!     including the ':'.
!
      integer(ip)           siztsq
      parameter           ( siztsq = 5 )

!
!     Integer codes of characters appearing in test clusters.
!
      integer(ip)           int000
      parameter           ( int000 = 0 )

      integer(ip)           int010
      parameter           ( int010 = 10 )

      integer(ip)           int013
      parameter           ( int013 = 13 )

      integer(ip)           int016
      parameter           ( int016 = 16 )

      integer(ip)           int129
      parameter           ( int129 = 129 )

      integer(ip)           int206
      parameter           ( int206 = 206 )

!
!
!     Local Variables
!
      character*(sizdlm)    locdlm
      character*(sizend)    loclnd
      character*(sizend)    locrnd
      character*(sizstr)    locstr
      character*(siztsq)    testsq ( numtst )

!
!     Non-printing character values.
!
      character*(1)         asc000
      character*(1)         asc010
      character*(1)         asc013
      character*(1)         asc016
      character*(1)         asc129
      character*(1)         asc206

      integer(ip)           i

      logical               first

!
!     Saved Variables
!
      save                  first
      save                  locdlm
      save                  loclnd
      save                  locrnd
      save                  locstr

!
!     Data Statements
!
      data   first       / .true.   /

!
!     Set up the components of the FTP validation string that
!     are not supposed to change for forward and backward
!     compatibility.
!
      data   locdlm      / ':'      /
      data   loclnd      / 'FTPSTR' /
      data   locrnd      / 'ENDFTP' /

!
!     On the first invocation initialize the string values.
!
      if ( first ) then
!
!        Convert the integer parameters to their non-printing ASCII
!        equivalents.
!
         asc000 = char( int000 )
         asc010 = char( int010 )
         asc013 = char( int013 )
         asc016 = char( int016 )
         asc129 = char( int129 )
         asc206 = char( int206 )

!
!        Now build the individual components of the test clusters.
!        Make certain the first component begins and ends with a ':',
!        and that the remaining pieces end in ':'. If you intend to
!        add some clusters, then append them to the end of the
!        sequence so as not to break the existing detection code.
!

!
!        Cluster #1 : <CR> - <13> - Macintosh Line Terminator
!
         testsq(1) = locdlm // asc013 // locdlm

!
!        Cluster #2 : <LF> - <10> - Unix Line Terminator
!
         testsq(2) = asc010 // locdlm

!
!        Cluster #3 : <CR><LF> - <10><13> - Microsoft Line Terminator
!
         testsq(3) = asc013 // asc010 // locdlm

!
!        Cluster #4 : <13><0>
!
         testsq(4) = asc013 // asc000 // locdlm

!
!        Cluster #5 : <129> - Macintosh Permutation of Parity Codes
!
         testsq(5) = asc129 // locdlm

!
!        Cluster #6 : <16><206>
!
         testsq(6) = asc016 // asc206 // locdlm

!
!        Sample cluster addition code follows
!
!        Cluster #7 : <xxx> - Description
!
!        TESTSQ(7) = ASCxxx // ... // LOCDLM
!

!
!        Now build the local copy of TSTCOM, LOCSTR. First clear the
!        uninitialized contents.
!
         locstr = ' '

         do i = 1, numtst

!
!           Append TESTSQ(I) to LOCSTR to properly construct the
!           test component of the FTP validation string.
!
            call suffix ( testsq(i), 0, locstr )

         end do

!
!        Prevent execution of this initialization code after first pass.
!
         first = .false.

      end if
!
!     Copy the local copies of the FTP string components to the
!     arguments passed in from the caller.
!
      tstcom = locstr
      lend   = loclnd
      rend   = locrnd
      delim  = locdlm

      return
      end
