!$Procedure POOL ( Maintain a pool of kernel variables )

      subroutine pool ( fname,  unit,   name,   names,  nnames, &
                        agent,  n,      values, found,  update, &
                        start,  room,   cvals,  ivals,  type, &
                        uwvars, uwptrs, uwpool, uwagnt, usrctr )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Maintain a pool of variables read from SPICE ASCII kernel files.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations

      implicit none

      include              'zzctr.inc'

      integer(ip)           maxvar
      parameter           ( maxvar =  26003 )

      integer(ip)           maxval
      parameter           ( maxval = 400000 )

      integer(ip)           maxlin
      parameter           ( maxlin =  15000 )

      integer(ip)           maxchr
      parameter           ( maxchr =     80 )

      integer(ip)           maxlen
      parameter           ( maxlen =     32 )

      integer(ip)           maxagt
      parameter           ( maxagt =   1000 )

      integer(ip)           mxnote
      parameter           ( mxnote =  maxvar * 5 )

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      integer(ip)           lbpool
      parameter           ( lbpool = -5 )


      character(*)          fname
      integer(ip)           unit
      character(*)          name
      character(*)          names     ( * )
      integer(ip)           nnames
      character(*)          agent
      integer(ip)           n
      real(wp)              values    ( * )
      logical               found
      logical               update
      integer(ip)           start
      integer(ip)           room
      character(*)          cvals     ( * )
      integer(ip)           ivals     ( * )
      character(*)          type
      character(*)          uwvars    ( lbcell : * )
      integer(ip)           uwptrs    ( * )
      integer(ip)           uwpool    ( 2, lbcell : * )
      character(*)          uwagnt    ( * )
      integer(ip)           usrctr    ( ctrsiz )

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     FNAME      I   LDPOOL
!     UNIT       I   WRPOOL
!     NAME       I   RTPOOL, EXPOOL, GIPOOL, GDPOOL, GCPOOL, PCPOOL,
!                    PDPOOL, PIPOOL, DTPOOL, SZPOOL, DVPOOL, GNPOOL
!     NAMES      I   SWPOOL
!     NNAMES     I   SWPOOL
!     AGENT      I   CVPOOL, DWPOOL, SWPOOL
!     N         I-O  RTPOOL, GIPOOL, GCPOOL, GDPOOL, DTPOOL, PCPOOL,
!                    PDPOOL, PIPOOL, LMPOOL, SZPOOL, GNPOOL
!     VALUES    I-O  RTPOOL  GDPOOL, PDPOOL
!     FOUND      O   RTPOOL, EXPOOL, GIPOOL, GCPOOL, GDPOOL, DTPOOL,
!                    SZPOOL, GNPOOL
!     UPDATE     O   CVPOOL, ZZPCTRCK
!     START      I   GIPOOL, GDPOOL, GCPOOL, GNPOOL
!     ROOM       I   GIPOOL, GDPOOL, GCPOOL. GNPOOL
!     CVALS     I-O  GCPOOL, PCPOOL, LMPOOL, GNPOOL
!     IVALS     I-O  GIPOOL, PIPOOL
!     TYPE       O   DTPOOL
!     UWVARS     O   ZZVUPOOL
!     UWPTRS     O   ZZVUPOOL
!     UWPOOL     O   ZZVUPOOL
!     UWAGNT     O   ZZVUPOOL
!     USRCTR    I-O  ZZPCTRCK
!
!     MAXVAR     P   (All)
!     MAXLEN     P   (All)
!     MAXVAL     P   (All)
!     MAXAGT     P   (All)
!     MXNOTE     P   (All)
!     BEGDAT     P   WRPOOL
!     BEGTXT     P   WRPOOL
!     CTRSIZ     P   ZZPCTRCK
!
! $ Detailed_Input
!
!     See the ENTRY points for a discussion of their arguments.
!
! $ Detailed_Output
!
!     See the ENTRY points for a discussion of their arguments.
!
! $ Parameters
!
!     MAXVAR   is the maximum number of variables that the
!              kernel pool may contain at any one time.
!              MAXVAR should be a prime number.
!
!              Here's a list of primes that should make
!              it easy to upgrade MAXVAR when/if the need arises.
!
!                  103
!                  199
!                  307
!                  401
!                  503
!                  601
!                  701
!                  751
!                  811
!                  911
!                 1013
!                 1213
!                 1303
!                 1511
!                 1811
!                 1913
!                 2003
!                 2203
!                 2503
!                 2803
!                 3203
!                 3607
!                 4001
!                 4507
!                 4801
!                 5003 Current Value
!                 6007
!                 6521
!                 7001
!                 7507
!                 8009
!                 8501
!                 9001
!                 9511
!                10007
!                10501
!                11003
!                11503
!
!
!     MAXLEN   is the maximum length of the variable names that
!              can be stored in the kernel pool (also set in
!              zzrvar.f).
!
!     MAXVAL   is the maximum number of distinct values that
!              may belong to the variables in the kernel pool.
!              Each variable must have at least one value, and
!              may have any number, so long as the total number
!              does not exceed MAXVAL. MAXVAL must be at least
!              as large as MAXVAR.
!
!     MAXAGT   is the maximum number of agents that can be
!              associated with a given kernel variable.
!
!     MAXCHR   is the maximum number of characters that can be
!              stored in a component of a string valued kernel
!              variable.
!
!     MXNOTE   is the maximum sum of the sizes of the sets of
!              agents in the range of the mapping that associates
!              with each watched kernel variable a set of agents
!              that "watch" that variable.
!
!     MAXLIN   is the maximum number of character strings that
!              can be stored as data for kernel pool variables.
!
!     CTRSIZ   is the dimension of the counter array used by
!              various SPICE subsystems to uniquely identify
!              changes in their states. This parameter is
!              defined in the private include file 'zzctr.inc'.
!
! $ Exceptions
!
!     1)  If POOL is called directly, the error SPICE(BOGUSENTRY) is
!         signaled.
!
! $ Files
!
!     See the ENTRY points for a discussion of their arguments.
!
! $ Particulars
!
!     POOL should never be called directly, but should instead be
!     accessed only through its entry points.
!
!     The purpose of this routine is to maintain a pool of variables
!     read from ASCII kernel files. The following entry points may be
!     used to access the pool.
!
!           CLPOOL         Clears the pool.
!
!           LDPOOL         Loads the variables from a kernel file into
!                          the pool.
!
!           RTPOOL         Returns the value of a variable from
!                          the pool. (Obsolete use GDPOOL)
!
!           EXPOOL         Confirms the existence of a numeric
!                          variable in the pool.
!
!           WRPOOL         Writes the contents of the pool to an
!                          ASCII kernel file.
!
!           SWPOOL         Sets up a "watcher" on a variable so that
!                          various "agents" can be notified when a
!                          variable has been updated.
!
!           CVPOOL         Indicates whether or not an agent's
!                          variable has been updated since the last
!                          time an agent checked with the pool.
!
!           GCPOOL         Returns the value of a string valued
!                          variable in the pool.
!
!           GDPOOL         Returns the d.p. value of a numeric valued
!                          variable in the pool.
!
!           GIPOOL         Returns the integer value of a numeric valued
!                          variable in the pool.
!
!           DTPOOL         Returns the attributes of a variable in the
!                          pool.
!
!           PCPOOL         Allows the insertion of a character variable
!                          directly into the kernel pool without
!                          supplying a text kernel.
!
!           PDPOOL         Allows the insertion of a double precision
!                          variable directly into the kernel pool
!                          without supplying a text kernel.
!
!           PIPOOL         Allows the insertion of an integer variable
!                          directly into the kernel pool without
!                          supplying a text kernel.
!
!           LMPOOL         Similar to LDPOOL, but the text kernel is
!                          stored in an array of strings instead of an
!                          external file.
!
!           SZPOOL         allows run time retrieval of kernel pool
!                          memory parameters.
!
!           DVPOOL         allows deletion of a specific variable from
!                          the kernel pool. (CLPOOL deletes all
!                          variables from the kernel pool.)
!
!           GNPOOL         assists in determining which variables are
!                          defined in the kernel pool via variable name
!                          template matching.
!
!           DWPOOL         deletes a watch from the watcher system.
!
!     Nominally, the kernel pool contains up to MAXVAR separate
!     variables, up to MAXVAL numeric values, and up to MAXLIN string
!     values. The names of the individual variables may contain up to
!     MAXLEN characters.
!
! $ Examples
!
!     The following code fragment demonstrates how the data from
!     several kernel files can be loaded into a kernel pool. After the
!     pool is loaded, the values in the pool are written to a kernel
!     file.
!
!     C
!     C     Store in an array the names of the kernel files whose
!     C     values will be loaded into the kernel pool.
!     C
!           FNAME (1) = 'AXES.KER'
!           FNAME (2) = 'GM.KER'
!           FNAME (3) = 'LEAP_SECONDS.KER'
!
!     C
!     C     Clear the kernel pool. (This is optional.)
!     C
!           CALL CLPOOL
!
!     C
!     C     Load the variables from the three kernel files into the
!     C     the kernel pool.
!     C
!           DO I = 1, 3
!             CALL LDPOOL ( FNAME (I) )
!           END DO
!
!     C
!     C     We can examine the values associated with any d.p. variable
!     C     in the kernel pool using GDPOOL.
!     C
!           CALL GDPOOL ( VARIABLE, START, ROOM, NVALS, VALUES, FOUND )
!
!     C
!     C     Open the text file 'NEWKERNEL.KER'.
!     C
!           CALL TXTOPN ( NEWKERNEL.KER', UNIT )
!
!     C
!     C     Write the values in the kernel pool to the file.
!     C
!           CALL WRPOOL ( UNIT )
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
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     R.E. Thurman       (JPL)
!     F.S. Turner        (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 10.2.0, 27-AUG-2021 (JDR) (BVS) (NJB)
!
!        Changed input argument name KERNEL to FNAME in entry point
!        LDPOOL for consistency with other routines.
!
!        Edited the header of the umbrella routine and all its entry
!        points.
!
!        Updated SZPOOL $Detailed_Input section.
!
!-    SPICELIB Version 10.1.0, 14-JUL-2014 (NJB) (BVS)
!
!        Updated header of WRPOOL to improve accuracy of
!        that routine's output description.
!
!        Updated header of CVPOOL to improve accuracy of
!        the description of the output argument UPDATE.
!
!        Updated header of CLPOOL to improve the description
!        of behavior of the watcher subsystem.
!
!        Last update was 17-JAN-2014 (BVS) (NJB)
!
!        Increased key POOL parameters as follows:
!
!           MAXVAR    5003 ->  26003
!           MAXVAL  200000 -> 400000
!           MAXLIN    4000 ->  15000
!
!        Decreased MXNOTE factor F (MXNOTE=MAXVAR*F) from 10 to 5.
!
!        Updated the main umbrella argument list to include the POOL
!        state counter. Added the private entry point ZZPCTRCK allowing
!        other routines to check their saved POOL state counter against
!        the current POOL state counter to detect and act on the POOL
!        state change.
!
!        Updated $Index_Entries sections of entry points PCPOOL, PDPOOL,
!        and PIPOOL.
!
!-    SPICELIB Version 10.0.0, 24-MAY-2010 (EDW) (NJB)
!
!        Added an error check on the length of the kernel pool variable
!        name argument in:
!
!           PCPOOL
!           PDPOOL
!           PIPOOL
!
!        to enforce the variable name length does not exceed MAXLEN.
!
!        Increased MAXVAL to 200000.
!
!-    SPICELIB Version 9.0.0, 19-MAR-2009 (NJB)
!
!        Added watch deletion entry point DWPOOL and private entry
!        point ZZVUPOOL. Re-implemented watcher system to improve
!        efficiency, particularly of watch deletion. Bug fix: corrected
!        watcher overflow detection logic in SWPOOL. Updated header
!        code examples to use TXTOPN instead of GETLUN and a Fortran
!        OPEN statement; also to use GDPOOL instead of RTPOOL, except in
!        the header of RTPOOL itself.
!
!        Code examples in SWPOOL and CVPOOL were updated to handle
!        kernel pool fetch failures.
!
!        Existing entry points modified as part of this update were:
!
!           POOL
!           CLPOOL
!           CVPOOL
!           DTPOOL
!           DVPOOL
!           EXPOOL
!           GCPOOL
!           GDPOOL
!           GIPOOL
!           GNPOOL
!           LDPOOL
!           LMPOOL
!           PCPOOL
!           PDPOOL
!           PIPOOL
!           RTPOOL
!           SWPOOL
!           WRPOOL
!
!        Code examples using RTPOOL were updated to use GDPOOL, except
!        in the header of RTPOOL itself. Code examples using GETLUN and
!        an in-line Fortran OPEN statement were updated to use TXTOPN.
!
!        Various typos in comments throughout this file were fixed.
!
!
!-    SPICELIB Version 8.3.0, 22-DEC-2004 (NJB)
!
!        Fixed bug in DVPOOL. Made corrections to comments in
!        other entry points. The updated routines are DTPOOL,
!        DVPOOL, EXPOOL, GCPOOL, GDPOOL, GIPOOL, RTPOOL.
!
!-    SPICELIB Version 8.2.0, 24-JAN-2003 (BVS)
!
!        Increased MAXVAL to 40000.
!
!-    SPICELIB Version 8.1.0, 13-MAR-2001 (FST) (NJB)
!
!        Increased kernel pool size and agent parameters. MAXVAR is now
!        5003, MAXVAL is 10000, MAXLIN is 4000, MXNOTE is 2000, and
!        MAXAGT is 1000.
!
!        Modified Fortran output formats used in entry point WRPOOL to
!        remove list-directed formatting. This change was made to
!        work around problems with the way f2c translates list-
!        directed I/O.
!
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
!        to the routine.
!
!        The entry point RTPOOL should now be regarded as obsolete
!        and is maintained solely for backward compatibility with
!        existing routines that make use of it.
!
!-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
!
!        The entry points SWPOOL and CVPOOL were added.
!
!-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
!
!        Increased value of parameter MAXVAL to 5000 to accommodate
!        storage of SCLK coefficients in the kernel pool.
!
!-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
!
!        All entry points except POOL and CLPOOL now initialize the
!        pool if it has not been done yet.
!
!-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
!
!        Added declaration of FAILED. FAILED is checked in the
!        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
!
!-    SPICELIB Version 2.0.0, 18-OCT-1989 (RET)
!
!       A FAILED test was inserted into the control of the DO-loop which
!       reads in each kernel variable in LDPOOL.
!
!-    SPICELIB Version 1.2.0, 09-MAR-1989 (HAN)
!
!        Parameters BEGDAT and BEGTXT have been moved into the
!        $Declarations section.
!
!-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
!
!        Parameters MAXVAR, MAXVAL, MAXLEN moved into $Declarations.
!        (Actually, MAXLEN was implicitly 32 characters, and has only
!        now been made an explicit---and changeable---limit.)
!
!        Declaration of unused function FAILED removed.
!
!-    SPICELIB Version 1.0.0, 08-JAN-1989 (IMU)
!
!-&


! $ Index_Entries
!
!     MAINTAIN a pool of kernel variables
!
!-&


! $ Revisions
!
!-    SPICELIB Version 8.3.0, 22-DEC-2004 (NJB)
!
!        Fixed bug in DVPOOL. Made corrections to comments in
!        other entry points. The updated routines are DTPOOL,
!        DVPOOL, EXPOOL, GCPOOL, GDPOOL, GIPOOL, RTPOOL.
!
!-    SPICELIB Version 8.2.0, 24-JAN-2003 (BVS)
!
!        Increased MAXVAL to 40000.
!
!-    SPICELIB Version 8.1.0, 13-MAR-2001 (FST) (NJB)
!
!        Increased kernel pool size and agent parameters. MAXVAR is now
!        5003, MAXVAL is 10000, MAXLIN is 4000, MXNOTE is 2000, and
!        MAXAGT is 1000.
!
!        Modified Fortran output formats used in entry point WRPOOL to
!        remove list-directed formatting. This change was made to
!        work around problems with the way f2c translates list-
!        directed I/O.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
!        to the routine.
!
!        The entry point RTPOOL should now be regarded as obsolete
!        and is maintained solely for backward compatibility with
!        existing routines that make use of it.
!
!        The basic data structure used to maintain the list of
!        variable names and values was replaced with a hash table
!        implementation. Data and names are accessed by means
!        of a hash function and linked lists of pointers to existing
!        variable names and data values.
!
!-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
!
!        The entry points SWPOOL (set watch on a pool variable)
!        and CVPOOL (check variable for update) so that routines
!        that buffer data stored in the kernel pool can fetch
!        that data only when it is updated.
!
!        Also the control of initializations was modified to be
!        consistent with other SPICELIB practices.
!
!        Finally, the revision history was upgraded so that the
!        version number increases over time. This wasn't true
!        before. In addition some early revision data that referred to
!        pre-SPICELIB modifications were removed. This editing of
!        the version numbers makes it unlikely that anyone can track
!        down which previous version of this routine they have by
!        looking at the version number. The best way to determine
!        the routine you had previously is to compare the dates
!        stored in the Version line of the routine.
!
!-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
!
!        Increased value of parameter MAXVAL to 5000 to accommodate
!        storage of SCLK coefficients in the kernel pool.
!
!        Also, changed version number in previous revisions entry
!        from SPICELIB Version 2.0.0 to SPICELIB Version 2.0.0. The
!        last version entry in the $Version section had been
!        Version 1.0.0, dated later than the entry for `version 2'
!        in the $Revisions section!
!
!-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
!
!        All entry points except POOL and CLPOOL now initialize the
!        pool if it has not been done yet.
!
!-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
!
!        Added declaration of FAILED. FAILED is checked in the
!        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
!
!-    SPICELIB Version 2.0.0, 18-OCT-1989 (RET)
!
!       A FAILED test was inserted into the control of the DO-loop which
!       reads in each kernel variable.
!
!       Previously, if the error action 'RETURN' had been set by a
!       calling program, and the call to RDKNEW by LDPOOL failed,
!       then execution would continue through LDPOOL, with SPICELIB
!       routines returning upon entry. This meant that the routine
!       RDKVAR never got a chance to set the EOF flag, which was the
!       only control of the DO-loop. An infinite loop resulted in such
!       cases. The FAILED test resolves that situation.
!
!-    SPICELIB Version 1.2.0, 9-MAR-1989 (HAN)
!
!        Parameters BEGDAT and BEGTXT have been moved into the
!        $Declarations section.
!
!-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
!
!        Parameters MAXVAR, MAXVAL, MAXLEN moved into $Declarations.
!        (Actually, MAXLEN was implicitly 32 characters, and has only
!        now been made an explicit---and changeable---limit.)
!
!        Declaration of unused function FAILED removed.
!
!-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           bsrchc
      integer(ip)           cardc
      integer(ip)           intmax
      integer(ip)           intmin
      integer(ip)           lnknfn
      integer(ip)           lnknxt
      integer(ip)           lnktl
      integer(ip)           lstltc
      integer(ip)           rtrim
      integer(ip)           sizec
      integer(ip)           lastnb


      logical               elemc
      logical               eqstr
      logical               failed
      logical               matchi
      logical               return

!
!     Private SPICELIB functions
!
      integer(ip)           zzhash

!
!     Local Parameters
!
      character(1)          quote
      parameter           ( quote = '''' )

      integer(ip)           lnsize
      parameter           ( lnsize = 132 )

!
!     The next two variables are for use in traversing linked lists.
!
      integer(ip)           prev
      parameter           ( prev = 2 )

      integer(ip)           next
      parameter           ( next = 1 )



!
!     Local variables
!

!
!     Because some environments (such as the SUN) are too stupid to
!     treat the backslash character correctly we have to go through
!     some gyrations to put it into a variable in a "portable" way.
!     This is the reason for the following block of declarations.
!     Admittedly this is bizarre, but it works.
!
      integer(ip)           mrklen
      parameter           ( mrklen = 10 )



      character(mrklen)     begdat
      character(mrklen)     begtxt


!
!     The following is the hash table used for holding kernel pool
!     variables.  Here's the basic structure:
!
!     The function ZZHASH computes the address of the head of a linked
!     list that contains the collisions for the range of ZZHASH.
!
!     The head node of the collision lists is stored in NAMLST.
!
!     If NAMLST has a value zero then
!
!        there is no name corresponding to that value of the
!        hash function.
!
!     If NAMLST is non-zero then
!
!        it is the head node of the list of names that have been
!        stored so far.
!
!        The list of addresses of names is stored in NMPOOL.
!        The names that have been stored so far are in PNAMES.
!
!     The data associated with  PNAMES is pointed to by DATLST
!     and CHPOOL or DPPOOL.  If a name of interest is stored in
!     PNAMES(I) then the DATLST(I) points to the first data node
!     associated with the name.
!
!     If DATLST(I) is less than zero then
!
!        its opposite is the address of the first node of
!        character data associated with PNAMES(I).
!
!     If DATLST(I) is positive then
!
!        it points to the address of the first node of numeric
!        data associated with PNAMES(I).
!
!     If DATLST(I) is zero
!
!        there is no data associated with PNAMES(I).
!
!
!     The arrays DPPOOL and CHPOOL are linked list pools that
!     give the address lists of values associated with a name.
!
!     The actual data is stored in DPVALS and CHVALS.
!
!     Here's a picture of how this all works.
!
!
!                                             Linked list Pool
!                                             of HASH collisions
!                       NAMLST                  NMPOOL         PNAME
!                     +------------+          +---------+    +--------+
!                     |            |          |         |    |        |
!                     +------------+ if not 0 +---------+    +--------+
!  ZZHASH( NAME ) --->|  Head Node | ---.     |         |    |        |
!                     +------------+    |     +---------+    +--------+
!                                       |     |         |    |        |
!                                       |     +---------+    +--------+
!                                       `-->  |Head of  |    |Name    |
!                                             |collision|    |corresp.|
!                                             |list for | -. |to head |
!                                             | NAME    |  | |of list |
!                                             +---------+  | +--------+
!                                             |         |  | |        |
!                                             +---------+  | +--------+
!                                             |         |  | |        |
!                                             +---------+  | +--------+
!                                             |Next Node|<-' |NextName|
!                                             +---------+etc.+--------+
!                                                  .              .
!                                                  .              .
!                                                  .              .
!                                             +---------+    +--------+
!                                             |         |    |        |
!                                             +---------+    +--------+
!
!
!
!
!      Linked       Variable    Heads of
!      List Pool     Names      Data lists
!       NMPOOL       PNAME       DATLST
!     +--------+   +--------+   +---------+          Head of linked list
!     |        |   |        |   |         |     .--> in DPPOOL linked
!     +--------+   +--------+   +---------+    |     list pool
!     |        |   |        |   |         |    |
!     +--------+   +--------+   +---------+    | Positive Value
!     |        |<->|        |<->|         |---<
!     +--------+   +--------+   +---------+    |
!     |        |   |        |   |         |    | Negative Value
!     +--------+   +--------+   +---------+    |
!     |        |   |        |   |         |    `--> Opposite of head
!     +--------+   +--------+   +---------+          of linked list
!     |        |   |        |   |         |          in CHPOOL linked
!     +--------+   +--------+   +---------+          list pool.
!
!
!
!
!
!      Linked                Values
!      List Pool             of data
!       DPPOOL (CHPOOL)      DPVALS (CHVALS)
!     +------------+         +------------+
!     |            |         |            |
!     +------------+         +------------+
!     |            |         |            |
!     +------------+         +------------+
!     | HEAD       |--. <--> | head value |
!     +------------+  |      +------------+
!     |            |  |      |            |
!     +------------+  |      +------------+
!     |            |  |      |            |
!     +------------+  |      +------------+
!     | Node 2     |<-' <--> | 2nd value  |
!     +------------+ etc.    +------------+
!     |            |         |            |
!     +------------+         +------------+
!     |            |         |            |
!     +------------+         +------------+
!     |            |         |            |
!     +------------+         +------------+
!     |            |         |            |
!     +------------+         +------------+
!     |            |         |            |
!     +------------+         +------------+
!     |            |         |            |
!     +------------+         +------------+
!
!
      character(maxlen)     pnames   (           maxvar )
      integer(ip)           namlst   (           maxvar )
      integer(ip)           nmpool   ( 2, lbpool:maxvar )
      integer(ip)           datlst   (           maxvar )

      integer(ip)           chpool   ( 2, lbpool:maxlin )
      integer(ip)           dppool   ( 2, lbpool:maxval )
      character(maxchr)     chvals   (           maxlin )
      real(wp)              dpvals   (           maxval )

!
!     The WT... variables make up the data structure that
!     maps variables to their associated agents (WTAGNT).
!     A diagram of the watcher data structure is shown below.
!
!      Watched     Heads of     Agent linked  Agent names
!      variables   agent lists  list pool
!       WTVARS       WTPTR        WTPOOL        WTAGNT
!     +--------+   +--------+   +---------+   +---------+
!     |        |   |        |   |         |   |         |
!     +--------+   +--------+   +---------+   +---------+
!     |        |   |        |   |         |   |         |
!     +--------+   +--------+   +---------+   +---------+
!     |        |<->|        |<->|         |<->|         |
!     +--------+   +--------+   +---------+   +---------+
!     |        |   |        |   |         |   |         |
!     +--------+   +--------+   +---------+   +---------+
!     |        |   |        |   |         |   |         |
!     +--------+   +--------+   +---------+   +---------+
!     |        |   |        |   |         |   |         |
!     +--------+   +--------+   +---------+   +---------+
!
!
      character(maxlen)     wtvars   (    lbcell : maxvar )
      integer(ip)           wtpool   ( 2, lbpool : mxnote )
      character(maxlen)     wtagnt   ( mxnote )
      integer(ip)           wtptrs   ( maxvar )

!
!     Agents contains the list of agents that need to be notified
!     about updates to their variables.  NOTIFY and ACTIVE are both
!     temporary sets.
!
!     These variables are declared with the size MXNOTE because
!     they must be able to hold the largest possible number
!     of agents that could be associated with a kernel variable.
!
      character(maxlen)     active   ( lbcell:mxnote )
      character(maxlen)     agents   ( lbcell:mxnote )
      character(maxlen)     notify   ( lbcell:mxnote )

!
!     First is our initialization flag.
!
      logical               first

!
!     POOL state counter.
!
      integer(ip)           subctr ( ctrsiz )

!
!     The remaining local variables...
!
      character(2)          finish
      character(lnsize)     cvalue
      character(lnsize)     line
      character(maxlen)     varnam

      real(wp)              big
      real(wp)              dvalue
      real(wp)              small

      integer(ip)           agnode
      integer(ip)           avail
      integer(ip)           begin
      integer(ip)           chnode
      integer(ip)           code
      integer(ip)           datahd
      integer(ip)           dnode
      integer(ip)           dpnode
      integer(ip)           free
      integer(ip)           head
      integer(ip)           hits
      integer(ip)           i
      integer(ip)           iostat
      integer(ip)           iquote
      integer(ip)           j
      integer(ip)           k
      integer(ip)           linnum
      integer(ip)           lookat
      integer(ip)           margin
      integer(ip)           nameat
      integer(ip)           need
      integer(ip)           nfetch
      integer(ip)           nnode
      integer(ip)           nnodes
      integer(ip)           node
      integer(ip)           nptrs
      integer(ip)           nvars
      integer(ip)           nw
      integer(ip)           r
      integer(ip)           space
      integer(ip)           tail
      integer(ip)           tofree
      integer(ip)           varlen

      logical               chr
      logical               dp
      logical               eof
      logical               gotit
      logical               isquot
      logical               noagnt
      logical               succes
      logical               vector

!
!     Save EVERYTHING.
!
      save

!
!     Initial values
!
      data                  first / .true. /

!
!     Set up the definition of our in-line functions.
!
      isquot(code) = code == iquote


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     This routine should never be called. If this routine is called,
!     an error is signaled.
!
      call setmsg ( 'POOL: You have called an entry which performs '  // &
                    'performs no run-time function. This may '        // &
                    'indicate a bug. Please check the documentation ' // &
                    'for the subroutine POOL.' )

      call sigerr ( 'SPICE(BOGUSENTRY)' )

      return





!$Procedure CLPOOL ( Clear the pool of kernel variables )

      entry clpool

! $ Abstract
!
!     Remove all kernel variables from the kernel pool. Watches
!     on kernel variables are retained.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     None.
!
! $ Detailed_Input
!
!     None.
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
!     Error free.
!
!     1)  All known agents (those established through the SPICELIB
!         routine SWPOOL) will be "notified" that their watched
!         variables have been updated whenever CLPOOL is called.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     CLPOOL clears the pool of kernel variables maintained by
!     the kernel POOL subsystem. All the variables in the pool are
!     deleted. However, all watcher information is retained.
!
!     Each watched variable will be regarded as having been updated.
!     Any agent associated with that variable will have a notice
!     posted for it indicating that its watched variable has been
!     updated.
!
!     Application programs can delete watches by calling the SPICELIB
!     routine DWPOOL. See the header of that routine for details.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) This code example demonstrates how to assign values to kernel
!        pool variables, how to check for the existence of kernel pool
!        variables and how to clear the kernel pool, i.e. how to delete
!        all variable assignments loaded into the kernel pool.
!
!        Place a value into the kernel pool and check for the variable
!        to which the value has been assigned. Clear the kernel pool
!        and check for that variable again.
!
!        Example code begins here.
!
!
!              PROGRAM CLPOOL_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      DVALS ( 1 )
!
!              INTEGER               N
!
!              LOGICAL               FOUND
!
!
!        C
!        C     Place a value into the kernel pool. Recall the routines
!        C     for direct insertion of pool assignments have arrays for
!        C     input.
!        C
!              DVALS( 1 ) = -666.D0
!              CALL PDPOOL ( 'TEST_VAR', 1, DVALS )
!
!        C
!        C     Check for the variable assignment to TEST_VAR.
!        C
!              CALL GDPOOL ( 'TEST_VAR', 1, 1, N, DVALS, FOUND )
!
!              WRITE(*,'(A)') 'First call to GDPOOL:'
!
!              IF ( FOUND ) THEN
!
!                 WRITE(*,'(A,F8.2)') '   TEST_VAR value:', DVALS(1)
!
!              ELSE
!
!                 WRITE(*,'(A)') '   TEST_VAR not in kernel pool.'
!
!              END IF
!
!        C
!        C     Now clear the kernel pool.
!        C
!              CALL CLPOOL ()
!
!        C
!        C     Again, check for the TEST_VAR assignment.
!        C
!              CALL GDPOOL ( 'TEST_VAR', 1, 1, N, DVALS, FOUND )
!
!              WRITE(*,'(A)') 'Second call to GDPOOL:'
!
!              IF ( FOUND ) THEN
!
!                 WRITE(*,'(A,F6.2)') '   TEST_VAR value:', DVALS(1)
!
!              ELSE
!
!                 WRITE(*,'(A)') '   TEST_VAR not in kernel pool.'
!
!              END IF
!
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        First call to GDPOOL:
!           TEST_VAR value: -666.00
!        Second call to GDPOOL:
!           TEST_VAR not in kernel pool.
!
!
! $ Restrictions
!
!     1)  This routine should not be used to unload kernels that
!         have been loaded via FURNSH.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 8.2.1, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!        Routine declared error free.
!
!-    SPICELIB Version 8.2.0, 01-JUL-2014 (NJB) (BVS)
!
!        Description of behavior of watcher subsystem was expanded.
!
!        Last update was 30-JUL-2013 (BVS)
!
!           Updated to increment POOL state counter.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        Watcher update code was re-written for compatibility
!        with new watcher system implementation. Updated $Restrictions
!        header section. Updated code example to use TXTOPN.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        This entry point clears the string valued variables as well as
!        the numeric valued variables.
!
!-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
!
!        The entry points SWPOOL and CVPOOL were added.
!
!-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
!
!        All entry points except POOL and CLPOOL now initialize the
!        pool if it has not been done yet.
!
!-    SPICELIB Version 1.0.0, 08-JAN-1989 (IMU)
!
!-&


! $ Index_Entries
!
!     CLEAR the pool of kernel variables
!
!-&


! $ Revisions
!
!-    SPICELIB Version 8.2.0, 01-JUL-2014 (NJB) (BVS)
!
!        Updated to increment POOL state counter.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        Watcher update code was re-written for compatibility
!        with new watcher system implementation.
!
!        ZZNWPOOL is called to update the list of agents
!        to notify of watched variable updates.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        This entry point clears the string valued variables as well as
!        the numeric valued variables.
!
!-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
!
!        The entry points SWPOOL (set watch on a pool variable)
!        and CVPOOL (check variable for update) so that routines
!        that buffer data stored in the kernel pool can fetch
!        that data only when it is updated.
!
!
!        Also the control of initializations was modified to be
!        consistent with other SPICELIB practices.
!
!        Finally, the revision history was upgraded so that the
!        version number increases over time. This wasn't true
!        before. In addition some early revision data that referred to
!        pre-SPICELIB modifications were removed. This editing of
!        the version numbers makes it unlikely that anyone can track
!        down which previous version of this routine they have by
!        looking at the version number. The best way to determine
!        the routine you had previously is to compare the dates
!        stored in the Version line of the routine.
!
!-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
!
!        All entry points except POOL and CLPOOL now initialize the
!        pool if it has not been done yet.
!
!-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return() ) return


!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Increment POOL state counter.
!
      call zzctrinc ( subctr )

!
!     Wipe out all of the PNAMES data.
!
      do i = 1, maxvar
         namlst(i) = 0
         datlst(i) = 0
         pnames(i) = ' '
      end do
!
!     Free up all of the space in all of the linked list pools, except
!     for the watcher pool.
!
      call lnkini ( maxvar, nmpool )
      call lnkini ( maxval, dppool )
      call lnkini ( maxlin, chpool )

      do i = 1, cardc ( wtvars )
!
!        Union the update set AGENTS with the set of agents
!        associated with the Ith watched variable.
!
         call zznwpool ( wtvars(i), wtvars, wtptrs, wtpool, &
                         wtagnt,    active, notify, agents )
      end do

      return





!$Procedure LDPOOL ( Load variables from a kernel file into the pool )

      entry ldpool ( fname )

! $ Abstract
!
!     Load the variables contained in a NAIF ASCII kernel file into the
!     kernel pool.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         FNAME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of the text kernel file.
!
! $ Detailed_Input
!
!     FNAME    is the name of the text kernel file whose variables will
!              be loaded into the pool.
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
!     1)  If an I/O error occurs while opening or reading a text kernel,
!         the error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If any text kernel parsing error occurs, the error is signaled
!         by a routine in the call tree of this routine.
!
!     3)  If a kernel pool overflow is detected, an error is signaled by
!         a routine in the call tree of this routine.
!
! $ Files
!
!     See FNAME in $Detailed_Input.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) The following program demonstrates how to load the variables
!        contained in a NAIF ASCII kernel file into the kernel pool
!        and how to determine the properties of a stored kernel
!        variable.
!
!        The program prompts for text kernel name and for the name of
!        a kernel variable. If the variable is present in the kernel
!        pool, the dimension and type of the variable are displayed.
!
!
!        Example code begins here.
!
!
!              PROGRAM LDPOOL_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               RTRIM
!
!        C
!        C     Local constants
!        C
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 256 )
!
!              INTEGER               KVNMLN
!              PARAMETER           ( KVNMLN = 33 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(FILSIZ)    FNAME
!              CHARACTER*(KVNMLN)    VARNAM
!              CHARACTER*(1)         VTYPE
!
!              INTEGER               N
!
!              LOGICAL               FOUND
!
!        C
!        C     Prompt for the name of a text-kernel file.
!        C
!              CALL PROMPT ( 'Enter text-kernel name        > ', FNAME )
!
!        C
!        C     Load the kernel. The same operation could be done using
!        C     a FURNSH call.
!        C
!              CALL LDPOOL ( FNAME )
!
!              CALL PROMPT ( 'Enter name of kernel variable > ',
!             .               VARNAM )
!
!              CALL DTPOOL ( VARNAM, FOUND, N, VTYPE )
!
!              IF ( FOUND ) THEN
!                 WRITE(*,*) ' '
!                 WRITE(*,*) 'Properties of variable ',
!             .               VARNAM(:RTRIM(VARNAM)), ':'
!                 WRITE(*,*) ' '
!                 WRITE(*,*) '   Size:   ', N
!
!                 IF ( VTYPE .EQ. 'C' ) THEN
!
!                    WRITE(*,*) '   Type:   Character'
!
!                 ELSE
!
!                    WRITE(*,*) '   Type:   Numeric'
!                 END IF
!
!              ELSE
!
!                 WRITE(*,*) VARNAM,
!             .              ' is not present in the kernel pool.'
!
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the PCK file gm_de431.tpc to ask for the
!        variable 'BODY000_GMLIST', the output was:
!
!
!        Enter text-kernel name        > gm_de431.tpc
!        Enter name of kernel variable > BODY000_GMLIST
!
!         Properties of variable BODY000_GMLIST:
!
!            Size:             65
!            Type:   Numeric
!
!
! $ Restrictions
!
!     1)  Normally SPICE applications should load kernels via the
!         FURNSH routine.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     R.E. Thurman       (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 8.3.0, 17-AUG-2021 (JDR)
!
!        Changed input argument name KERNEL to FNAME for consistency
!        with other routines.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!-    SPICELIB Version 8.2.0, 30-JUL-2013 (BVS)
!
!        Updated to increment POOL state counter.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        Watcher update code was re-written for compatibility
!        with new watcher system implementation.
!
!        Filled out $Exceptions section of header, which previously
!        contained only the word "None."
!
!        Updated code example to use TXTOPN.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        In addition much greater error checking is performed on
!        the input file to guarantee valid inputs.
!
!-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
!
!        The entry points SWPOOL and CVPOOL were added.
!
!-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
!
!        Increased value of parameter MAXVAL to 5000 to accommodate
!        storage of SCLK coefficients in the kernel pool.
!
!-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
!
!        All entry points except POOL and CLPOOL now initialize the
!        pool if it has not been done yet.
!
!-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
!
!        Added declaration of FAILED. FAILED is checked in the
!        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
!
!-    SPICELIB Version 2.0.0, 18-OCT-1989 (RET)
!
!        A FAILED test was inserted into the control of the DO-loop
!        which reads in each kernel variable in LDPOOL.
!
!-    SPICELIB Version 1.2.0, 09-MAR-1989 (HAN)
!
!        Parameters BEGDAT and BEGTXT have been moved into the
!        $Declarations section.
!
!-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
!
!        Parameters MAXVAR, MAXVAL, MAXLEN moved into $Declarations.
!        (Actually, MAXLEN was implicitly 32 characters, and has only
!        now been made an explicit---and changeable---limit.)
!
!        Declaration of unused function FAILED removed.
!
!-    SPICELIB Version 1.0.0, 08-JAN-1989 (IMU)
!
!-&


! $ Index_Entries
!
!     LOAD variables from a text kernel file into the pool
!
!-&


! $ Revisions
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
!        to the routine.
!
!        The entry point RTPOOL should now be regarded as obsolete
!        and is maintained solely for backward compatibility with
!        existing routines that make use of it.
!
!        The basic data structure used to maintain the list of
!        variable names and values was replaced with a hash table
!        implementation. Data and names are accessed by means
!        of a hash function and linked lists of pointers to existing
!        variable names and data values.
!
!        In addition much greater error checking is performed on
!        the input file to guarantee valid inputs.
!
!-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
!
!        The entry points SWPOOL (set watch on a pool variable)
!        and CVPOOL (check variable for update) so that routines
!        that buffer data stored in the kernel pool can fetch
!        that data only when it is updated.
!
!        In addition, the revision history was upgraded so that the
!        version number increases over time. This wasn't true
!        before. In addition some early revision data that referred to
!        pre-SPICELIB modifications were removed. This editing of
!        the version numbers makes it unlikely that anyone can track
!        down which previous version of this routine they have by
!        looking at the version number. The best way to determine
!        the routine you had previously is to compare the dates
!        stored in the Version line of the routine.
!
!-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
!
!        Increased value of parameter MAXVAL to 5000 to accommodate
!        storage of SCLK coefficients in the kernel pool.
!
!        Also, changed version number in previous revisions entry
!        from SPICELIB Version 2.0.0 to SPICELIB Version 2.0.0. The
!        last version entry in the $Version section had been
!        Version 1.0.0, dated later than the entry for `version 2'
!        in the $Revisions section!
!
!-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
!
!        All entry points except POOL and CLPOOL now initialize the
!        pool if it has not been done yet.
!
!-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
!
!        Added declaration of FAILED. FAILED is checked in the
!        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
!
!-    SPICELIB Version 2.0.0, 18-OCT-1989 (RET)
!
!       A FAILED test was inserted into the control of the DO-loop which
!       reads in each kernel variable.
!
!       Previously, if the error action 'RETURN' had been set by a
!       calling program, and the call to RDKNEW by LDPOOL failed,
!       then execution would continue through LDPOOL, with SPICELIB
!       routines returning upon entry. This meant that the routine
!       RDKVAR never got a chance to set the EOF flag, which was the
!       only control of the DO-loop. An infinite loop resulted in such
!       cases. The FAILED test resolves that situation.
!
!-    SPICELIB Version 1.2.0, 9-MAR-1989 (HAN)
!
!        Parameters BEGDAT and BEGTXT have been moved into the
!        $Declarations section.
!
!-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
!
!        Parameters MAXVAR, MAXVAL, MAXLEN moved into $Declarations.
!        (Actually, MAXLEN was implicitly 32 characters, and has only
!        now been made an explicit---and changeable---limit.)
!
!        Declaration of unused function FAILED removed.
!
!-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Increment POOL state counter.
!
      call zzctrinc ( subctr )

!
!     Open the kernel file and read the first variable.
!
      call rdknew ( fname )
      call zzrvar ( namlst, &
                    nmpool, pnames, datlst, &
                    dppool, dpvals, &
                    chpool, chvals, &
                    varnam, eof )

!
!     Read the variables in the file, one at a time.
!
      do while (  ( .not. eof ) .and. (.not. failed() )   )

         if ( varnam /= ' ' ) then
!
!           See if this variable is being watched; if it is, add its
!           associated agents to the list of AGENTS to be notified of a
!           watched variable update.
!
            if (  elemc( varnam, wtvars )  ) then
!
!              Union the update set AGENTS with the set of agents
!              associated with the variable NAME.
!
               call zznwpool ( varnam, wtvars, wtptrs, wtpool, &
                               wtagnt, active, notify, agents )

            end if


         end if


         call zzrvar ( namlst, &
                       nmpool, pnames, datlst, &
                       dppool, dpvals, &
                       chpool, chvals, &
                       varnam, eof )

      end do
!
!     We need to make sure that the kernel file gets closed.  Normally
!     the calling tree of ZZRVAR take care of this, but if a parsing
!     or syntax error occurs there,  ZZRVAR just returns and the
!     closing of the kernel is never handled.  This takes care
!     of the problem.  If the file has been closed already, this
!     doesn't hurt anything.
!
      call cltext (  fname  )
      return




!$Procedure RTPOOL ( Return the value of a pooled kernel variable )

      entry rtpool ( name, n, values, found )

! $ Abstract
!
!     Return the value of a kernel variable from the kernel pool.
!
!     This routine is maintained only for backward compatibility.
!     It should be regarded as obsolete. Use one of the entry points
!     GDPOOL, GIPOOL or GCPOOL in its place.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     INTEGER               N
!     DOUBLE PRECISION      VALUES   ( * )
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the variable whose value is to be returned.
!     N          O   Number of values associated with NAME.
!     VALUES     O   Values associated with NAME.
!     FOUND      O   .TRUE. if variable is in pool.
!
! $ Detailed_Input
!
!     NAME     is the name of the variable whose values are to be
!              returned. If the variable is not in the pool, FOUND
!              will be .FALSE.
!
! $ Detailed_Output
!
!     N        is the number of values associated with NAME.
!              If NAME is not in the pool, no value is given to
!              N.
!
!     VALUES   is the array of values associated with NAME.
!              If NAME is not in the pool, no values are given to
!              the elements of VALUES.
!
!     FOUND    is .TRUE. if the variable is in the pool, .FALSE. if it
!              is not.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the output argument VALUES is not large enough to hold all
!         of the values of the kernel variable designated by NAME,
!         then memory will be corrupted. RTPOOL cannot detect this
!         error.
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
!     The following code fragment demonstrates how the data from
!     several kernel files can be loaded into a kernel pool. After the
!     pool is loaded, the values in the pool are written to a kernel
!     file.
!
!
!     C
!     C     Store in an array the names of the kernel files whose
!     C     values will be loaded into the kernel pool.
!     C
!           FNAME (1) = 'AXES.KER'
!           FNAME (2) = 'GM.KER'
!           FNAME (3) = 'LEAP_SECONDS.KER'
!
!     C
!     C     Clear the kernel pool. (This is optional.)
!     C
!           CALL CLPOOL
!
!     C
!     C     Load the variables from the three kernel files into the
!     C     the kernel pool.
!     C
!           DO I = 1, 3
!             CALL LDPOOL ( FNAME (I) )
!           END DO
!
!     C
!     C     We can examine the values associated with any variable
!     C     in the kernel pool using RTPOOL.
!     C
!           CALL RTPOOL ( VARIABLE, NUMVAL, VALUES, FOUND )
!
!     C
!     C     Open the new text file 'NEWKERNEL.KER'.
!     C
!           CALL TXTOPN ( 'NEWKERNEL.KER', UNIT )
!
!     C
!     C     Write the values in the kernel pool to the file.
!     C
!           CALL WRPOOL ( UNIT )
!
! $ Restrictions
!
!     See $Exceptions section.
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
!
! $ Version
!
!-    SPICELIB Version 8.1.1, 17-AUG-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        ZZPINI call was updated for compatibility
!        with new watcher system implementation.
!
!        Updated code example to use TXTOPN.
!
!-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
!
!        Corrected an in-line comment relating to finding the
!        head node of the conflict resolution list for NAME.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
!        to the routine.
!
!        The entry point RTPOOL should now be regarded as obsolete
!        and is maintained solely for backward compatibility with
!        existing routines that make use of it.
!
!-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
!
!        All entry points except POOL and CLPOOL now initialize the
!        pool if it has not been done yet.
!
!-    SPICELIB Version 1.0.0, 08-JAN-1989 (IMU)
!
!-&


! $ Index_Entries
!
!     RETURN the value of a pooled kernel variable
!
!-&


! $ Revisions
!
!-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
!
!        All entry points except POOL and CLPOOL now initialize the
!        pool if it has not been done yet.
!
!-    SPICELIB Version 1.0.0, 08-JAN-1989 (IMU)
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return() ) return


!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Compute the hash value of this name.
!
      lookat = zzhash ( name )

!
!     Now see if there is a non-empty conflict resolution list for the
!     input string NAME.  If so, NAMLST(LOOKAT) contains the head node
!     of the conflict resolution list; this node is a positive value.
!
      if ( namlst(lookat) == 0 ) then

         found = .false.
         return

      end if
!
!     If were are still here NAMLST(LOOKAT) is the first node of
!     a conflict resolution list.  See if the NAME corresponding
!     to this node is the one we are looking for.
!
      node   = namlst( lookat      )
      succes = name == pnames(node)

      do while ( .not. succes )

         node = nmpool ( next, node )

         if ( node < 0 ) then

            found = .false.
            return

         end if

         succes = name == pnames(node)

      end do
!
!     If you get to this point, the variable NAME is present in the
!     list of names at PNAMES(NODE), ABS( DATLST(NODE) ) points to the
!     head of a linked list of values for this NAME.
!
!     However, recall that RTPOOL can only return d.p. values.
!     DATLST(NODE) is the head of a d.p. list of values if it
!     is positive.  We use negative values to point to character
!     values.
!
      if ( datlst(node) <= 0 ) then

         found = .false.

      else

         found = .true.
         n     =  0
         node  =  datlst(node)

         do while ( node > 0 )
            n         = n + 1
            values(n) = dpvals(     node)
            node      = dppool(next,node)
         end do

      end if

      return




!$Procedure EXPOOL ( Confirm the existence of a pooled kernel variable )

      entry expool ( name, found )

! $ Abstract
!
!     Confirm the existence of a numeric kernel variable in the kernel
!     pool.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of a numeric kernel variable.
!     FOUND      O   .TRUE. when the variable is in the pool.
!
! $ Detailed_Input
!
!     NAME     is the name of the numeric kernel variable whose
!              existence in the kernel pool is to be checked.
!
! $ Detailed_Output
!
!     FOUND    is .TRUE. whenever the specified variable is included
!              in the pool.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine determines whether or not a numeric kernel pool
!     variable exists. It does not detect the existence of
!     string valued kernel pool variables.
!
!     A better routine for determining the existence of numeric kernel
!     pool variables is the routine DTPOOL which determines the
!     existence, size and type of kernel pool variables.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) The following code example demonstrates how to use EXPOOL
!        to confirm the existence of numeric kernel pool variables.
!        In the example, we will look for different variables;
!        some of them numeric, some string valued and some not
!        present in the kernel pool.
!
!        Use the kernel shown below; an IK defining two keywords
!        used to provide data for an instrument with NAIF ID -999001.
!
!
!           KPL/IK
!
!           File name: expool_ex1.ti
!
!           The keyword below define the three frequencies used by a
!           hypothetical instrument (NAIF ID -999001). They correspond
!           to three filters: red, green and blue. Frequencies are
!           given in micrometers.
!
!           \begindata
!
!              INS-999001_FREQ_RGB   = (  0.65,  0.55, 0.475 )
!              INS-999001_FREQ_UNITS = ( 'MICROMETERS'       )
!
!           \begintext
!
!
!           End of IK
!
!
!        Example code begins here.
!
!
!              PROGRAM EXPOOL_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         IKNAME
!              PARAMETER           ( IKNAME = 'expool_ex1.ti' )
!
!              INTEGER               KPVNLN
!              PARAMETER           ( KPVNLN = 32 )
!
!              INTEGER               NKPVNM
!              PARAMETER           ( NKPVNM = 3  )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(KPVNLN)    KEYWRD ( NKPVNM )
!
!              INTEGER               I
!
!              LOGICAL               FOUND
!
!        C
!        C     Define the variable names
!        C
!              DATA                  KEYWRD   /
!             .                             'INS-999001_FREQ_RGB',
!             .                             'NOT_IN_THE_POOL',
!             .                             'INS-999001_FREQ_UNITS' /
!
!        C
!        C     Load the instrument kernel.
!        C
!              CALL FURNSH ( IKNAME )
!
!              DO I = 1, NKPVNM
!
!        C
!        C        Check if the variable is numeric and present
!        C        in the kernel pool.
!        C
!                 CALL EXPOOL ( KEYWRD(I), FOUND )
!
!                 WRITE(*,*) 'Variable name: ', KEYWRD(I)
!
!                 IF ( FOUND ) THEN
!
!                    WRITE(*,*) '   It is numeric and exists in the '
!             .              // 'kernel pool.'
!
!                 ELSE
!
!                    WRITE(*,*) '   Either it is not numeric or it is '
!             .              // 'not in the kernel pool.'
!
!                 END IF
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Variable name: INS-999001_FREQ_RGB
!            It is numeric and exists in the kernel pool.
!         Variable name: NOT_IN_THE_POOL
!            Either it is not numeric or it is not in the kernel pool.
!         Variable name: INS-999001_FREQ_UNITS
!            Either it is not numeric or it is not in the kernel pool.
!
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
!
! $ Version
!
!-    SPICELIB Version 8.1.1, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!        Updated the header to reflect that only numeric variables
!        present in the kernel pool will cause the routine to return
!        .TRUE.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        ZZPINI call was updated for compatibility
!        with new watcher system implementation.
!
!        Fixed typos.
!
!-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
!
!        Corrected an in-line comment relating to finding the
!        head node of the conflict resolution list for NAME.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
!        to the routine.
!
!-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
!
!        All entry points except POOL and CLPOOL now initialize the
!        pool if it has not been done yet.
!
!-    SPICELIB Version 1.0.0, 08-JAN-1989 (IMU)
!
!-&


! $ Index_Entries
!
!     CONFIRM the existence of a pooled numeric kernel variable
!
!-&


! $ Revisions
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        ZZPINI call was updated for compatibility
!        with new watcher system implementation.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
!        to the routine.
!
!        The entry point RTPOOL should now be regarded as obsolete
!        and is maintained solely for backward compatibility with
!        existing routines that make use of it.
!
!        The basic data structure used to maintain the list of
!        variable names and values was replaced with a hash table
!        implementation. Data and names are accessed by means
!        of a hash function and linked lists of pointers to existing
!        variable names and data values.
!
!-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
!
!        All entry points except POOL and CLPOOL now initialize the
!        pool if it has not been done yet.
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return() ) return


!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Compute the hash value of this name.
!
      lookat = zzhash ( name )

!
!     Now see if there is a non-empty conflict resolution list for the
!     input string NAME.  If so, NAMLST(LOOKAT) contains the head node
!     of the conflict resolution list; this node is a positive value.
!
      if ( namlst(lookat) == 0 ) then

         found = .false.
         return

      end if
!
!     If were are still here NAMLST(LOOKAT) is the first node of
!     a conflict resolution list.  See if the NAME corresponding
!     to this node is the one we are looking for.
!
      node   = namlst ( lookat      )
      succes = name == pnames(node)

      do while ( .not. succes )

         node = nmpool ( next, node )

         if ( node < 0 ) then

            found = .false.
            return

         end if

         succes = name == pnames(node)

      end do
!
!     If you get to this point, the variable NAME is present in the
!     list of names at PNAMES(NODE), ABS( DATLST(NODE) ) points to the
!     head of a linked list of values for this NAME.
!
!     However, recall that EXPOOL indicates the existence only of
!     d.p. values.
!
      found = datlst(node) > 0

      return





!$Procedure WRPOOL ( Write the variables in pool to a specified unit )

      entry wrpool ( unit )

! $ Abstract
!
!     Write to a specified unit a set of "keyword = value" assignments
!     for all currently defined kernel variables. The assignments
!     constitute a text kernel from which the current state of the
!     kernel pool can be restored.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations
!
!     INTEGER        UNIT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Logical unit to which the variables in the pool
!                    will be written.
!
! $ Detailed_Input
!
!     UNIT     is the logical unit to which the variables in the pool
!              will be written.
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
!     1)  If writing a variable to the output file fails due to an I/O
!         error, the error SPICE(WRITEERROR) is signaled.
!
! $ Files
!
!     This routine writes to the specified logical unit the kernel
!     variables present in the kernel pool. Each variable consists of a
!     name and a set of associated values. The variables are written in
!     the form of a series of "keyword = value" assignments. The
!     assignments are preceded by a SPICE text kernel "\begindata"
!     marker.
!
!     The output of this routine, if written to a text file, is a SPICE
!     text kernel. The current contents of the kernel pool can be
!     restored by clearing the pool and then loading this text kernel.
!
!     If the values are to be written to an output kernel file, the
!     file should be opened with a logical unit determined by the
!     calling program.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     The following code fragment demonstrates how the data from
!     several kernel files can be loaded into a kernel pool. After the
!     pool is loaded, the values in the pool are written to a kernel
!     file.
!
!
!     C
!     C     Store in an array the names of the kernel files whose
!     C     values will be loaded into the kernel pool.
!     C
!           FNAME (1) = 'AXES.KER'
!           FNAME (2) = 'GM.KER'
!           FNAME (3) = 'LEAP_SECONDS.KER'
!
!     C
!     C     Clear the kernel pool. (This is optional.)
!     C
!           CALL CLPOOL
!
!     C
!     C     Load the variables from the three kernel files into the
!     C     the kernel pool.
!     C
!           DO I = 1, 3
!             CALL LDPOOL ( FNAME (I) )
!           END DO
!
!     C
!     C     We can examine the values associated with any double
!     C     precision variable in the kernel pool using GDPOOL.
!     C
!           CALL GDPOOL ( VARIABLE, 1, NMAX, NUMVAL, VALUES, FOUND )
!
!     C
!     C     Open the new text file 'NEWKERNEL.KER'.
!     C
!           CALL TXTOPN ( 'NEWKERNEL.KER', UNIT )
!
!     C
!     C     Write the values in the kernel pool to the file.
!     C
!           CALL WRPOOL ( UNIT )
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
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 8.1.2, 17-AUG-2021 (JDR) (BVS)
!
!        Edited the header to comply with NAIF standard.
!
!        Updated the $Exceptions section to document the actual error
!        handling of the routine.
!
!-    SPICELIB Version 8.1.1, 30-JUN-2014 (NJB)
!
!        Updated header to more accurately describe the output
!        of this routine.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        Updated code example to use TXTOPN.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued. Both types are supported
!        by WRPOOL.
!
!-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
!
!        Increased value of parameter MAXVAL to 5000 to accommodate
!        storage of SCLK coefficients in the kernel pool.
!
!-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
!
!        All entry points except POOL and CLPOOL now initialize the
!        pool if it has not been done yet.
!
!-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
!
!        Added declaration of FAILED. FAILED is checked in the
!        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
!
!-    SPICELIB Version 1.2.0, 09-MAR-1989 (HAN)
!
!        Parameters BEGDAT and BEGTXT have been moved into the
!        $Declarations section.
!
!-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
!
!        Parameters MAXVAR, MAXVAL, MAXLEN moved into $Declarations.
!        (Actually, MAXLEN was implicitly 32 characters, and has only
!        now been made an explicit---and changeable---limit.)
!
!        Declaration of unused function FAILED removed.
!
!-    SPICELIB Version 1.0.0, 08-JAN-1989 (IMU)
!
!-&


! $ Index_Entries
!
!     WRITE the values in pool to a specified unit
!
!-&


! $ Revisions
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        The basic data structure used to maintain the list of
!        variable names and values was replaced with a hash table
!        implementation. Data and names are accessed by means
!        of a hash function and linked lists of pointers to existing
!        variable names and data values.
!
!-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
!
!        Increased value of parameter MAXVAL to 5000 to accommodate
!        storage of SCLK coefficients in the kernel pool.
!
!-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
!
!        All entry points except POOL and CLPOOL now initialize the
!        pool if it has not been done yet.
!
!-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
!
!        Added declaration of FAILED. FAILED is checked in the
!        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
!
!-    SPICELIB Version 2.0.0, 18-OCT-1989 (RET)
!
!       A FAILED test was inserted into the control of the DO-loop which
!       reads in each kernel variable.
!
!       Previously, if the error action 'RETURN' had been set by a
!       calling program, and the call to RDKNEW by LDPOOL failed,
!       then execution would continue through LDPOOL, with SPICELIB
!       routines returning upon entry. This meant that the routine
!       RDKVAR never got a chance to set the EOF flag, which was the
!       only control of the DO-loop. An infinite loop resulted in such
!       cases. The FAILED test resolves that situation.
!
!-    SPICELIB Version 1.2.0, 9-MAR-1989 (HAN)
!
!        Parameters BEGDAT and BEGTXT have been moved into the
!        $Declarations section.
!
!-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
!
!        Parameters MAXVAR, MAXVAL, MAXLEN moved into $Declarations.
!        (Actually, MAXLEN was implicitly 32 characters, and has only
!        now been made an explicit---and changeable---limit.)
!
!        Declaration of unused function FAILED removed.
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Indicate the beginning of a data section.
!
      write ( unit, '(1X,A)', iostat=iostat ) begdat
      write ( unit, '(1X,A)', iostat=iostat )

      if ( iostat /= 0 ) then
         call ioerr ( 'writing a variable to the output ' &
         //           'kernel file ', &
                      ' ', &
                       iostat             )
         call sigerr ( 'SPICE(WRITEERROR)' )
         return
      end if

!
!     Next prepare for writing out the data.
!
      iquote = ichar ( quote    )
      margin = maxlen + 6

      do k = 1, maxvar
!
!        Get the head of this list.
!
         nnode = namlst(k)

         do while ( nnode > 0 )

            line   = pnames( nnode )
            datahd = datlst( nnode )
            dp     = datahd > 0
            chr    = datahd < 0
            dnode  = abs(datahd)
!
!           Determine whether or not this is a vector object.
!
            if ( dp ) then
               vector = dppool( next, dnode ) > 0
            else if ( chr ) then
               vector = chpool( next, dnode ) > 0
            else
               call setmsg ( 'This error is never supposed to ' &
               //            'occur. No data was available for ' &
               //            'the variable ''#''. ' )

               r = rtrim(pnames(nnode))
               call errch  ( '#', pnames(nnode)(1:r) )
               call sigerr ( 'SPICE(BUG)'           )
               return
            end if
!
!           If still here, then we can set up the beginning of this
!           output line.
!
            line(maxlen+2:) = '= '

            if ( vector ) then
               line(maxlen+4:) = '( '
            end if
!
!           Now fetch all of the data associated with this variable.
!           We'll write them out one per line.
!
            do while ( dnode > 0 )
!
!              Get the next data value and the address of the next node.
!
               if ( dp ) then
                  dvalue = dpvals(dnode)
                  dnode  = dppool(next,dnode)
               else
                  cvalue = quote
                  j      = 1
!
!                 We have to double up each of the quotes on output.
!                 For this reason we copy the letters one at a time
!                 into the output holding area CVALUE.
!
                  do i = 1, rtrim(chvals(dnode))
                     j           = j + 1
                     cvalue(j:j) = chvals(dnode)(i:i)

                     code = ichar(chvals(dnode)(i:i))

                     if ( isquot(code) ) then
                        j           = j+1
                        cvalue(j:j) = chvals(dnode)(i:i)
                     end if
                  end do

                  j           = j + 1
                  cvalue(j:j) = quote
                  dnode       = chpool(next,dnode)
               end if

!
!              We will need to properly finish off this write with
!              either a comma, a blank or a right parenthesis.
!
               if ( dnode > 0 ) then
                  finish = ', '
               else if ( vector ) then
                  finish = ' )'
               else
                  finish = ' '
               end if
!
!              Now write out our data.
!
               if ( dp ) then
                  write ( unit, '(1X,A,D25.17,A)', iostat=iostat ) &
                  line(1:margin), dvalue, finish
               else
                  write ( unit, '(1X,3A)', iostat=iostat ) &
                  line(1:margin), cvalue(1:j), finish
               end if

!
!              Check the IOSTAT code.  After all, that's why it's there.
!
               if ( iostat /= 0 ) then
                  call ioerr ( 'writing a variable to the output ' &
                  //           'kernel file ', &
                                ' ', &
                                iostat              )
                  call sigerr ( 'SPICE(WRITEERROR)' )
                  return
               end if

!
!              Blank out the output line so that we'll have
!              leading blanks for subsequent components of the
!              vector (if we are in fact writing one).
!
               line = ' '

            end do
!
!           Get the next name for this node:
!
            nnode = nmpool(next,nnode)

         end do
!
!        Get the next node (if there is one).
!
      end do


!
!     Indicate the beginning of a text section. Data sections and
!     text sections must alternate, even if the text section is blank.
!
      write ( unit, '(1X,A)', iostat=iostat )
      write ( unit, '(1X,A)', iostat=iostat ) begtxt

      if ( iostat /= 0 ) then
         call ioerr ( 'writing a variable to the output ' &
         //           'kernel file ', &
                        ' ', &
                        iostat              )
         call sigerr ( 'SPICE(WRITEERROR)' )
         return
      end if

      return




!$Procedure SWPOOL ( Set watch on a pool variable )

      entry swpool ( agent, nnames, names )

! $ Abstract
!
!     Add a name to the list of agents to notify whenever a member of
!     a list of kernel variables is updated.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         AGENT
!     INTEGER               NNAMES
!     CHARACTER*(*)         NAMES  ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     AGENT      I   The name of an agent to be notified after updates.
!     NNAMES     I   The number of variables to associate with AGENT.
!     NAMES      I   Variable names whose update causes the notice.
!
! $ Detailed_Input
!
!     AGENT    is the name of a routine or entry point (agency) that
!              will want to know when the kernel pool variables
!              designated by NAMES have been updated.
!
!     NNAMES   is the number of kernel pool variable names that will
!              be associated with AGENT.
!
!     NAMES    is an array of names of variables in the kernel pool.
!              Whenever any of these is updated, a notice will be
!              posted for AGENT so that one can quickly check
!              whether needed data has been modified.
!
!              Any kernel variable may be associated with multiple
!              agents; this call adds AGENT to each set of agents
!              associated with a member of NAMES.
!
!              The variables designated by NAMES need not exist in
!              the kernel pool at the time a watch is set.
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
!     1)  If sufficient room is not available to hold a new kernel
!         variable name, the error SPICE(KERVARSETOVERFLOW) is signaled.
!
!     2)  If sufficient room is not available to hold a new agent
!         name, the error SPICE(TOOMANYWATCHES) is signaled.
!
!     3)  If any kernel variable in the array NAMES is already watched
!         by MAXAGT agents, and AGENT is not already associated with
!         that kernel variable, the error SPICE(AGENTLISTOVERFLOW) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The kernel pool is a convenient place to store a wide
!     variety of data needed by routines in SPICELIB and routines
!     that interface with SPICELIB routines. However, when
!     a single name has a large quantity of data associated with
!     it, it becomes inefficient to constantly query the kernel
!     pool for values that are not updated on a frequent basis.
!
!     This entry point allows a routine to instruct the kernel pool
!     to post a message whenever a particular value gets updated.
!     In this way, a routine can quickly determine whether or not
!     data it requires has been updated since the last time the
!     data was accessed. This makes it reasonable to buffer
!     the data in local storage and update it only when
!     a variable in the kernel pool that affects this data has
!     been updated.
!
!     Note that SWPOOL has a side effect. Whenever a call to
!     SWPOOL is made, the agent specified in the calling sequence
!     is added to the list of agents that should be notified that
!     an update of its variables has occurred. In other words
!     the code
!
!         CALL SWPOOL ( AGENT, NNAMES, NAMES  )
!         CALL CVPOOL ( AGENT,         UPDATE )
!
!     will always return UPDATE as .TRUE.
!
!     This feature allows for a slightly cleaner use of SWPOOL and
!     CVPOOL as shown in the example below. Because SWPOOL
!     automatically loads AGENT into the list of agents to notify of
!     a kernel pool update, you do not have to include the code for
!     fetching the initial values of the kernel variables in the
!     initialization portion of a subroutine. Instead, the code for
!     the first fetch from the pool is the same as the code for
!     fetching when the pool is updated.
!
! $ Examples
!
!     Suppose that you have an application subroutine, MYTASK, that
!     needs to access a large data set in the kernel pool. If this
!     data could be kept in local storage and kernel pool queries
!     performed only when the data in the kernel pool has been
!     updated, the routine can perform much more efficiently.
!
!     The code fragment below illustrates how you might make use of this
!     feature.
!
!     C
!     C     On the first call to this routine establish those variables
!     C     that we will want to read from the kernel pool only when
!     C     new values have been established.
!     C
!           IF ( FIRST ) THEN
!
!              FIRST = .FALSE.
!              HAVE  = .FALSE.
!
!              CALL SWPOOL ( 'MYTASK', NNAMES, NAMES )
!
!           END IF
!
!     C
!     C     If any of the variables has been updated, fetch
!     C     it from the kernel pool. (Note that this also
!     C     handles getting variables for the first time.)
!     C     We use HAVE to indicate the fetch succeeded. If it
!     C     didn't, we need to attempt the fetch on the next
!     C     pass into this routine.
!     C
!           CALL CVPOOL ( 'MYTASK', UPDATE )
!
!           IF (  UPDATE  .OR (.NOT. HAVE ) ) THEN
!
!              CALL GDPOOL ( 'MYTASK_VAR_1', 1, M, N1, VALS1, FOUND(1) )
!              CALL GDPOOL ( 'MYTASK_VAR_2', 1, M, N2, VALS2, FOUND(2) )
!                      .
!                      .
!                      .
!              CALL GDPOOL ( 'MYTASK_VAR_N', 1, M, NN, VALSN, FOUND(N) )
!
!           END IF
!
!           IF ( FAILED() ) THEN
!                 .
!                 .
!              do something about the failure
!                 .
!                 .
!
!           ELSE
!
!              HAVE = .TRUE.
!
!           END IF
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 8.2.1, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 8.2.0, 30-JUL-2013 (BVS)
!
!        Updated to increment POOL state counter.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        This routine was re-written to work with the new
!        watcher system implementation. Several bugs related
!        to watch system overflow were fixed.
!
!        The code example was updated to handle kernel pool
!        fetch failure.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
!
!        The entry points SWPOOL and CVPOOL were added.
!
!-&


! $ Index_Entries
!
!     Watch for an update to a kernel pool variable
!     Notify a routine of an update to a kernel pool variable
!
!-&


! $ Revisions
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        This routine was re-written to work with the new
!        watcher system implementation.
!
!        Several bugs related to watch system overflow were fixed.
!        Now overflow error checks are performed *before* the
!        watcher system is updated, so a partial update won't
!        occur if there's not enough room for a full update.
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        The basic data structure used to maintain the list of
!        variable names and values was replaced with a hash table
!        implementation. Data and names are accessed by means
!        of a hash function and linked lists of pointers to existing
!        variable names and data values.
!
!-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
!
!        The entry points SWPOOL (set watch on a pool variable)
!        and CVPOOL (check variable for update) so that routines
!        that buffer data stored in the kernel pool can fetch
!        that data only when it is updated.
!
!        In addition, the revision history was upgraded so that the
!        version number increases over time. This wasn't true
!        before. In addition some early revision data that referred to
!        pre-SPICELIB modifications were removed. This editing of
!        the version numbers makes it unlikely that anyone can track
!        down which previous version of this routine they have by
!        looking at the version number. The best way to determine
!        the routine you had previously is to compare the dates
!        stored in the Version line of the routine.
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Increment POOL state counter. Although setting a watcher does not
!     change the POOL we will increment the POOL state counter to make
!     sure that the next call to CVPOOL with this watcher triggers the
!     initial update.
!
      call zzctrinc ( subctr )

!
!     Do all of the error checking we need to do BEFORE touching
!     the watcher data structure. We don't want to end up with
!     a partial update due to running out of room in mid-update.
!
!     First make sure we can handle any new kernel variable names.
!
      need = 0

      do i = 1, nnames

         if (  .not.  elemc( names(i), wtvars )  ) then
            need = need + 1
         end if

      end do

      space = sizec(wtvars) - cardc(wtvars)


      if ( need > space ) then

         call setmsg ( 'The watched kernel variable name ' &
         //            'list WTVARS has room for # more ' &
         //            'elements, so the # new names (in ' &
         //            'a list of # names) ' &
         //            'associated with agent # ' &
         //            'cannot be inserted.'              )
         call errint ( '#', space                         )
         call errint ( '#', need                          )
         call errint ( '#', nnames                        )
         call errch  ( '#', agent                         )
         call sigerr ( 'SPICE(KERVARSETOVERFLOW)'         )
         return

      end if


!
!     If the input agent is a new one for any member of NAMES,
!     make sure we have enough room to store this agent. Also
!     check for kernel variables that would have more than
!     MAXAGT agents watching them if this watch were established.
!
      need = 0

      do i = 1, nnames
!
!        Get the agents associated with NAMES(I). The output argument
!        ACTIVE is a SPICE set.
!
         call zzgapool ( names(i), wtvars, wtptrs, &
                         wtpool,   wtagnt, active )


         nfetch = cardc( active )


         noagnt =       ( nfetch ==   0               ) &
                   .or. ( .not.  elemc( agent, active ) )

         if ( noagnt ) then

            need = need + 1
!
!           Check the number of agents already associated with the
!           current kernel variable.
!
            if ( nfetch == maxagt ) then

               call setmsg ( 'The list of agents to notify when # '   // &
                             'is updated is too big. The maximum '    // &
                             'number of agents that any kernel'       // &
                             'pool variable can activate is #.'       )
               call errch  ( '#', names(i)                            )
               call errint ( '#', maxagt                              )
               call sigerr ( 'SPICE(TOOMANYWATCHES)'                  )
               return

            end if

         end if

      end do

!
!     See whether WTAGNT has enough room to set this watch.
!
      space = lnknfn( wtpool )

      if ( need > space ) then

         call setmsg ( 'The watched kernel variable agent ' &
         //            'list WTAGNT has room for # more ' &
         //            'elements, so the # new occurrences ' &
         //            'of agent # required for the input ' &
         //            'watch cannot be inserted.'          )
         call errint ( '#', space                           )
         call errint ( '#', need                            )
         call errch  ( '#', agent                           )
         call sigerr ( 'SPICE(AGENTLISTOVERFLOW)'           )
         return

      end if


!
!     All of the overflow checks have been done. We finally can
!     get on with setting the specified watch.
!
!     For each variable specified by the array NAMES, put AGENT
!     into its list of guys to be notified when a variable change
!     occurs.
!
      do i = 1, nnames
!
!        Get the agents associated with NAMES(I). The output argument
!        ACTIVE is a SPICE set.
!
         call zzgapool ( names(i), wtvars, wtptrs, &
                         wtpool,   wtagnt, active )

         nfetch = cardc( active )

!
!        Three things can happen now:
!
!           1) The kernel variable NAMES(I) is already watched by at
!              least one agent, but not by AGENT. We need to add AGENT
!              to the list of agents watching NAMES(I).
!
!           2) The kernel variable NAMES(I) isn't yet watched by any
!              agent, so we need to insert NAMES(I) into WTVARS, as
!              well as add AGENT to the (empty) list of agents watching
!              NAMES(I).
!
!           3) The kernel variable NAMES(I) is already watched by AGENT.
!              No action is needed.
!
!        We could get fancy and try to minimize the number of lines of
!        code required to handle the first two cases...but we won't.
!        We'll just take them one at a time.
!
!
         if ( nfetch > 0 ) then

            if (  .not.  elemc( agent, active )  ) then
!
!              Case 1: at least one agent is already watching NAMES(I),
!              but AGENT is not watching NAMES(I). We need the head of
!              the agent list for this kernel variable.
!
               j    = bsrchc ( names(i), cardc(wtvars), wtvars(1) )

               head = wtptrs(j)

!
!              Allocate a free node in the watch pool; append this node
!              to the tail of the agent list for the kernel variable;
!              we know that list is non-empty.
!
               call lnkan  ( wtpool, node )

               tail = lnktl( head,   wtpool )

               call lnkila ( tail,   node, wtpool )

!
!              Store the agent name at index NODE in the agent list.
!
               wtagnt( node ) = agent

!
!              The insertion is complete. We update AGENTS, which is
!              the set of agents to notify, at the end of this routine.
!
            end if

         else
!
!           Case 2: the kernel variable NAMES(I) isn't watched. Add it
!           the watcher system. We've already ensured that there's
!           room in WTVARS and WTAGNT and that the insertion won't give
!           NAMES(I) an excessive number of agents.
!
!           Let J be the insertion index in WTVARS. Since NAMES(I)
!           isn't yet a member of WTWARS, the insertion index will
!           always follow that of the last element in WTVARS
!           less than NAMES(I).
!
            j = 1 + lstltc ( names(i), cardc(wtvars), wtvars(1) )

!
!           Note that we don't use INSRTC to add NAMES(I) to WTVARS
!           because we need the insertion index, and we don't want
!           to execute a redundant search to find it.
!
!           We're now going to expand both the set WTVARS and the
!           parallel array WTPTRS by inserting new values at index J.
!           WTVARS(J) will receive the new kernel variable name
!           NAMES(I) and WTPTRS(J) will receive a new node in the watch
!           pool: this node provides an index into the agent list for
!           NAMES(I).
!
!           Let NVARS be the size of the array WTVARS(1:*) prior to
!           the insertion. NVARS will be updated by INSLAC.
!
!           NPTRS is the size of the associated pointer table WTPTRS.
!
            nvars = cardc( wtvars )
            nptrs = nvars

            call inslac ( names(i), 1, j, wtvars(1), nvars )

!
!           WTVARS is actually a set, so we must update its cardinality.
!
            call scardc ( nvars, wtvars )

!
!           Allocate a free node in the watch pool.
!
            call lnkan ( wtpool, node )

!
!           Now insert NODE in the pointer table WTPTRS at index J.
!
            call inslai ( node, 1, j, wtptrs, nptrs )

!
!           Store the agent name at index NODE in the agent list.
!
            wtagnt( node ) = agent

!
!           The insertion is complete. We update AGENTS, which is the
!           set of agents to notify, at the end of this routine.

         end if

      end do

!
!     We ALWAYS put this agent into the list of agents to be notified.
!
      call insrtc ( agent, agents )

!
!     That is all.
!
      return







!$Procedure CVPOOL ( Check variable in the pool for update)

      entry cvpool ( agent, update )

! $ Abstract
!
!     Indicate whether or not any watched kernel variables that have a
!     specified agent on their notification list have been updated.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     SYMBOLS
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         AGENT
!     LOGICAL               UPDATE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     AGENT      I   Name of the agent to check for notices.
!     UPDATE     O   .TRUE. if variables for AGENT have been updated.
!
! $ Detailed_Input
!
!     AGENT    is the name of a subroutine, entry point, or significant
!              portion of code that needs to access variables in the
!              kernel pool. Generally this agent will buffer these
!              variables internally and fetch them from the kernel
!              pool only when they are updated.
!
! $ Detailed_Output
!
!     UPDATE   is a logical flag that will be set to .TRUE. if the
!              variables in the kernel pool that are associated with
!              AGENT have been updated since the last call to CVPOOL.
!
!              UPDATE will be set to .TRUE. on the first call made for
!              the specified agent, whether or not the associated
!              variables have been updated since the agent was placed
!              on their notification list, as long as the agent is
!              associated with any watched variables.
!
! $ Parameters
!
!     See the umbrella subroutine POOL.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point allows the calling program to determine
!     whether or not variables associated with with AGENT have
!     been updated. Making use of this entry point in conjunction
!     with the entry point SWPOOL (set watch on pool variables)
!     modules can buffer kernel pool variables they need and
!     fetch values from the kernel pool only when variables have
!     been updated.
!
!     Note that the call to CVPOOL has a side effect.
!     Two consecutive calls to CVPOOL with the same
!     AGENT will always result in the UPDATE being .FALSE.
!     on the second call. In other words, if you embed
!     the following two lines of code in a piece of code
!
!        CALL CVPOOL ( AGENT, UPDATE )
!        CALL CVPOOL ( AGENT, UPDATE )
!
!     and then test UPDATE, it will be .FALSE. The idea is
!     that once a call to CVPOOL has been made, the
!     kernel pool has performed its duty and notified the
!     calling routine that one of the AGENT's variables
!     has been updated. Consequently, on the second call
!     to CVPOOL above, the kernel pool will not have any
!     updates to report about any of AGENT's variables.
!
!     If, on the other hand, you have code such as
!
!        CALL CVPOOL ( AGENT, UPDATE )
!        CALL LDPOOL ( 'MYFILE.DAT'  )
!        CALL CVPOOL ( AGENT, UPDATE )
!
!     the value of UPDATE will be true if one of the variables
!     associated with AGENT was updated by the call to
!     LDPOOL (and that variable has been specified as one
!     to watch by call a call to SWPOOL).
!
!     It should also be noted that any call to CVPOOL that
!     occurs immediately after a call to SWPOOL will result in
!     UPDATE being returned as .TRUE. In other words, code
!     such as shown below, will always result in the value
!     of UPDATE as being returned .TRUE.
!
!        CALL SWPOOL ( AGENT, NNAMES, NAMES  )
!        CALL CVPOOL ( AGENT,         UPDATE )
!
!     See the header for SWPOOL for a full discussion of this
!     feature.
!
! $ Examples
!
!     Suppose that you have an application subroutine, MYTASK, that
!     needs to access a large data set in the kernel pool. If this
!     data could be kept in local storage and kernel pool queries
!     performed only when the data in the kernel pool has been
!     updated, the routine can perform much more efficiently.
!
!     The code fragment below illustrates how you might make use of this
!     feature.
!
!     C
!     C     On the first call to this routine establish those variables
!     C     that we will want to read from the kernel pool only when
!     C     new values have been established.
!     C
!           IF ( FIRST ) THEN
!
!              FIRST = .FALSE.
!              HAVE  = .FALSE.
!
!              CALL SWPOOL ( 'MYTASK', NNAMES, NAMES )
!
!           END IF
!
!     C
!     C     If any of the variables has been updated, fetch
!     C     it from the kernel pool. (Note that this also
!     C     handles getting variables for the first time.)
!     C     We use HAVE to indicate the fetch succeeded. If it
!     C     didn't, we need to attempt the fetch on the next
!     C     pass into this routine.
!     C
!           CALL CVPOOL ( 'MYTASK', UPDATE )
!
!           IF (  UPDATE  .OR (.NOT. HAVE ) ) THEN
!
!              CALL GDPOOL ( 'MYTASK_VAR_1', 1, M, N1, VALS1, FOUND(1) )
!              CALL GDPOOL ( 'MYTASK_VAR_2', 1, M, N2, VALS2, FOUND(2) )
!                      .
!                      .
!                      .
!              CALL GDPOOL ( 'MYTASK_VAR_N', 1, M, NN, VALSN, FOUND(N) )
!
!           END IF
!
!           IF ( FAILED() ) THEN
!                 .
!                 .
!              do something about the failure
!                 .
!                 .
!
!           ELSE
!
!              HAVE = .TRUE.
!
!           END IF
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
!
! $ Version
!
!-    SPICELIB Version 8.1.2, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 8.1.1, 30-JUN-2014 (NJB)
!
!        Description of the output variable UPDATE now
!        mentions that the initial value of .TRUE. will
!        be returned after an agent is associated with
!        kernel variables.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        ZZPINI call was updated for compatibility
!        with new watcher system implementation.
!
!        The code example was updated to handle kernel pool
!        fetch failure.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
!
!        The entry points SWPOOL and CVPOOL were added.
!
!-&


! $ Index_Entries
!
!     Check the kernel pool for updated variables
!
!-&


! $ Revisions
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        ZZPINI call was updated for compatibility
!        with new watcher system implementation.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        The basic data structure used to maintain the list of
!        variable names and values was replaced with a hash table
!        implementation. Data and names are accessed by means
!        of a hash function and linked lists of pointers to existing
!        variable names and data values.
!
!-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
!
!        The entry points SWPOOL (set watch on a pool variable)
!        and CVPOOL (check variable for update) so that routines
!        that buffer data stored in the kernel pool can fetch
!        that data only when it is updated.
!
!        In addition, the revision history was upgraded so that the
!        version number increases over time. This wasn't true
!        before. In addition some early revision data that referred to
!        pre-SPICELIB modifications were removed. This editing of
!        the version numbers makes it unlikely that anyone can track
!        down which previous version of this routine they have by
!        looking at the version number. The best way to determine
!        the routine you had previously is to compare the dates
!        stored in the Version line of the routine.
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Check to see if our agent is on the list of agents to be
!     notified.  If it is, we take this agent off the list---he's
!     now considered to have been notified.
!
      update = elemc  ( agent, agents )

      if ( update ) then
         call removc ( agent, agents )
      end if


      return





!$Procedure GCPOOL (Get character data from the kernel pool)

      entry gcpool ( name, start, room, n, cvals, found )

! $ Abstract
!
!     Return the character value of a kernel variable from the
!     kernel pool.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     INTEGER               START
!     INTEGER               ROOM
!     INTEGER               N
!     CHARACTER*(*)         CVALS    ( * )
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the variable whose value is to be returned.
!     START      I   Which component to start retrieving for NAME
!     ROOM       I   The largest number of values to return.
!     N          O   Number of values returned for NAME.
!     CVALS      O   Values associated with NAME.
!     FOUND      O   .TRUE. if variable is in pool.
!
! $ Detailed_Input
!
!     NAME     is the name of the variable whose values are to be
!              returned. If the variable is not in the pool with
!              character type, FOUND will be .FALSE.
!
!     START    is the index of the first component of NAME to return.
!              If START is less than 1, it will be treated as 1. If
!              START is greater than the total number of components
!              available for NAME, no values will be returned (N will
!              be set to zero). However, FOUND will still be set to
!              .TRUE.
!
!     ROOM     is the maximum number of components that should be
!              returned for this variable. (Usually it is the amount
!              of ROOM available in the array CVALS). If ROOM is
!              less than 1 the error SPICE(BADARRAYSIZE) will be
!              signaled.
!
! $ Detailed_Output
!
!     N        is the number of values associated with NAME that
!              are returned. It will always be less than or equal
!              to ROOM.
!
!              If NAME is not in the pool with character type, no
!              value is given to N.
!
!     CVALS    is the array of values associated with NAME.
!              If NAME is not in the pool with character type, no
!              values are given to the elements of CVALS.
!
!              If the length of CVALS is less than the length of
!              strings stored in the kernel pool (see MAXCHR) the
!              values returned will be truncated on the right.
!
!     FOUND    is .TRUE. if the variable is in the pool and has
!              character type, .FALSE. if it is not.
!
! $ Parameters
!
!     MAXCHR   is the maximum number of characters that can be
!              stored in a component of a string valued kernel
!              variable. This value is currently 80.
!
! $ Exceptions
!
!     1)  If the value of ROOM is less than one, the error
!         SPICE(BADARRAYSIZE) is signaled.
!
!     2)  If CVALS has declared length less than the size of a
!         string to be returned, the value will be truncated on
!         the right. See MAXCHR for the maximum stored size of
!         string variables.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides the user interface to retrieving
!     character data stored in the kernel pool. This interface
!     allows you to retrieve the data associated with a variable
!     in multiple accesses. Under some circumstances this alleviates
!     the problem of having to know in advance the maximum amount
!     of space needed to accommodate all kernel variables.
!
!     However, this method of access does come with a price. It is
!     always more efficient to retrieve all of the data associated
!     with a kernel pool data in one call than it is to retrieve
!     it in sections.
!
!     See also the entry points GDPOOL and GIPOOL.
!
! $ Examples
!
!     The following code fragment demonstrates how the data stored
!     in a kernel pool variable can be retrieved in pieces.
!
!     First we need some declarations.
!
!        INTEGER               ROOM
!        PARAMETER           ( ROOM = 3 )
!
!        CHARACTER*(8)         VARNAM
!        CHARACTER*(3)         INDENT
!        INTEGER               START
!        INTEGER               N
!        LOGICAL               FOUND
!        CHARACTER*(80)        CVALS(ROOM)
!
!
!     Next load the data in the file 'typical.ker' into the
!     kernel pool.
!
!        CALL LDPOOL ( 'typical.ker' )
!
!     Next we shall print the values stored for the kernel pool
!     variable 'MYDATA'
!
!        VARNAM = 'MYDATA'
!        INDENT = ' '
!        START  =  1
!
!        CALL GCPOOL ( VARNAM, START, ROOM, N, CVALS, FOUND )
!
!        IF ( .NOT. FOUND )
!           WRITE (*,*) 'There is no string data available for MYDATA.'
!        ELSE
!
!           WRITE (*,*) 'Values for MYDATA.'
!           WRITE (*,*)
!
!           DO I = 1, N
!              WRITE (*,*) INDENT, CVALS(I)
!           END DO
!
!           DO WHILE ( N .EQ. ROOM )
!
!              START = START + N
!              CALL GCPOOL ( VARNAM, START, ROOM, N, CVALS, FOUND )
!
!              DO I = 1, N
!                 WRITE (*,*) INDENT, CVALS(I)
!              END DO
!
!           END DO
!
!        END IF
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
!
! $ Version
!
!-    SPICELIB Version 8.1.1, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added MAXCHR
!        description in $Parameters.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        ZZPINI call was updated for compatibility
!        with new watcher system implementation.
!
!-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
!
!        Corrected an in-line comment relating to finding the
!        head node of the conflict resolution list for NAME.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
!        to the routine.
!
!-&


! $ Index_Entries
!
!     RETURN the character value of a pooled kernel variable
!     RETURN the string value of a pooled kernel variable
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Perform the one obvious error check first.
!
      if ( room < 1 ) then

         call setmsg ( 'The amount of room specified as ' &
         //            'available for output in the output array ' &
         //            'was: #.  The amount of room must be ' &
         //            'positive. ' )


         call errint ( '#', room )
         call sigerr ( 'SPICE(BADARRAYSIZE)'  )
         return

      end if



!
!     Compute the hash value of this name.
!
      lookat = zzhash ( name )

!
!     Now see if there is a non-empty conflict resolution list for the
!     input string NAME.  If so, NAMLST(LOOKAT) contains the head node
!     of the conflict resolution list; this node is a positive value.
!
      if ( namlst(lookat) == 0 ) then

         found = .false.
         return

      end if
!
!     If were are still here NAMLST(LOOKAT) is the first node of
!     a conflict resolution list.  See if the NAME corresponding
!     to this node is the one we are looking for.
!
      node   = namlst( lookat      )
      succes = name == pnames(node)

      do while ( .not. succes )

         node = nmpool ( next, node )

         if ( node < 0 ) then

            found = .false.
            return

         end if

         succes = name == pnames(node)

      end do
!
!     If you get to this point, the variable NAME is present in the
!     list of names at PNAMES(NODE), ABS( DATLST(NODE) ) points to the
!     head of a linked list of values for this NAME.
!
      datahd = datlst(node)

      if ( datahd > 0 ) then

         n     =  0
         found = .false.
         return

      else if ( datahd == 0 ) then

         call setmsg ( 'This is never supposed to happen.  The ' &
         //            'requested name, ''#'', was found in the ' &
         //            'name list, but the pointer to the head ' &
         //            'of the data for this variable is zero. ' &
         //            'Please note your activities and report ' &
         //            'this error to NAIF. '   )
         call errch  ( '#', name(1:rtrim(name)) )
         call sigerr ( 'SPICE(BUG)'             )
         return

      end if

      found = .true.
      k     =  0
      n     =  0
      begin =  max( start, 1 )
      node  =  -datahd

      do while ( node > 0  )

         k = k + 1

         if ( k >= begin ) then
            n        = n+1
            cvals(n) = chvals(     node)

            if ( n == room ) return

         end if

         node = chpool(next,node)

      end do

      return








!$Procedure GDPOOL (Get d.p. values from the kernel pool)

      entry gdpool ( name, start, room, n, values, found )

! $ Abstract
!
!     Return the d.p. value of a kernel variable from the kernel pool.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     INTEGER               START
!     INTEGER               ROOM
!     INTEGER               N
!     DOUBLE PRECISION      VALUES   ( * )
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the variable whose value is to be returned.
!     START      I   Which component to start retrieving for NAME
!     ROOM       I   The largest number of values to return.
!     N          O   Number of values returned for NAME.
!     VALUES     O   Values associated with NAME.
!     FOUND      O   .TRUE. if variable is in pool.
!
! $ Detailed_Input
!
!     NAME     is the name of the variable whose values are to be
!              returned. If the variable is not in the pool with
!              numeric type, FOUND will be .FALSE.
!
!     START    is the index of the first component of NAME to return.
!              If START is less than 1, it will be treated as 1. If
!              START is greater than the total number of components
!              available for NAME, no values will be returned (N will
!              be set to zero). However, FOUND will still be set to
!              .TRUE.
!
!     ROOM     is the maximum number of components that should be
!              returned for this variable. (Usually it is the amount
!              of ROOM available in the array VALUES). If ROOM is
!              less than 1 the error SPICE(BADARRAYSIZE) will be
!              signaled.
!
! $ Detailed_Output
!
!     N        is the number of values associated with NAME that
!              are returned. It will always be less than or equal
!              to ROOM.
!
!              If NAME is not in the pool with numeric type, no value
!              is given to N.
!
!     VALUES   is the array of values associated with NAME.
!              If NAME is not in the pool with numeric type, no
!              values are given to the elements of VALUES.
!
!     FOUND    is .TRUE. if the variable is in the pool and has numeric
!              type, .FALSE. if it is not.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the value of ROOM is less than one, the error
!         SPICE(BADARRAYSIZE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides the user interface to retrieving
!     numeric data stored in the kernel pool. This interface
!     allows you to retrieve the data associated with a variable
!     in multiple accesses. Under some circumstances this alleviates
!     the problem of having to know in advance the maximum amount
!     of space needed to accommodate all kernel variables.
!
!     However, this method of access does come with a price. It is
!     always more efficient to retrieve all of the data associated
!     with a kernel pool data in one call than it is to retrieve
!     it in sections.
!
!     This routine should be used in place of RTPOOL when possible
!     as it avoids errors associated with writing data past the
!     end of an array.
!
!     See also the entry points GIPOOL and GCPOOL.
!
! $ Examples
!
!     The following code fragment demonstrates how the data stored
!     in a kernel pool variable can be retrieved in pieces.
!
!     First we need some declarations.
!
!        INTEGER               ROOM
!        PARAMETER           ( ROOM = 3 )
!
!        CHARACTER*(8)         VARNAM
!        CHARACTER*(3)         INDENT
!        INTEGER               START
!        INTEGER               N
!        LOGICAL               FOUND
!        DOUBLE PRECISION      VALUES(ROOM)
!
!
!     Next load the data in the file 'typical.ker' into the
!     kernel pool.
!
!
!
!        CALL LDPOOL ( 'typical.ker' )
!
!     Next we shall print the values stored for the kernel pool
!     variable 'MYDATA'
!
!        VARNAM = 'MYDATA'
!        INDENT = ' '
!        START  =  1
!
!        CALL GDPOOL ( VARNAM, START, ROOM, N, VALUES, FOUND )
!
!        IF ( .NOT. FOUND )
!           WRITE (*,*) 'There is no numeric data available for MYDATA.'
!        ELSE
!
!           WRITE (*,*) 'Values for MYDATA.'
!           WRITE (*,*)
!
!           DO I = 1, N
!              WRITE (*,*) INDENT, VALUES(I)
!           END DO
!
!           DO WHILE ( N .EQ. ROOM )
!
!              START = START + N
!              CALL GDPOOL ( VARNAM, START, ROOM, N, VALUES, FOUND )
!
!              DO I = 1, N
!                 WRITE (*,*) INDENT, VALUES(I)
!              END DO
!
!           END DO
!
!        END IF
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
!
! $ Version
!
!-    SPICELIB Version 8.1.1, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        ZZPINI call was updated for compatibility
!        with new watcher system implementation.
!
!-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
!
!        Corrected an in-line comment relating to finding the
!        head node of the conflict resolution list for NAME.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
!        to the routine.
!
!-&


! $ Index_Entries
!
!     RETURN the d.p. value of a pooled kernel variable
!     RETURN the numeric value of a pooled kernel variable
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Perform the one obvious error check first.
!
      if ( room < 1 ) then

         call setmsg ( 'The amount of room specified as ' &
         //            'available for output in the output array ' &
         //            'was: #.  The amount of room must be ' &
         //            'positive. ' )


         call errint ( '#', room )
         call sigerr ( 'SPICE(BADARRAYSIZE)'  )
         return

      end if


!
!     Compute the hash value of this name.
!
      lookat = zzhash ( name )

!
!     Now see if there is a non-empty conflict resolution list for the
!     input string NAME.  If so, NAMLST(LOOKAT) contains the head node
!     of the conflict resolution list; this node is a positive value.
!
      if ( namlst(lookat) == 0 ) then

         found = .false.
         return

      end if
!
!     If were are still here NAMLST(LOOKAT) is the first node of
!     a conflict resolution list.  See if the NAME corresponding
!     to this node is the one we are looking for.
!
      node   = namlst( lookat      )
      succes = name == pnames(node)

      do while ( .not. succes )

         node = nmpool ( next, node )

         if ( node < 0 ) then

            found = .false.
            return

         end if

         succes = name == pnames(node)

      end do
!
!     If you get to this point, the variable NAME is present in the
!     list of names at PNAMES(NODE), ABS( DATLST(NODE) ) points to the
!     head of a linked list of values for this NAME.
!
      datahd = datlst(node)

      if ( datahd < 0 ) then

         n     =  0
         found = .false.
         return

      else if ( datahd == 0 ) then

         call setmsg ( 'This is never supposed to happen.  The ' &
         //            'requested name, ''#'', was found in the ' &
         //            'name list, but the pointer to the head ' &
         //            'of the data for this variable is zero. ' &
         //            'Please note your activities and report ' &
         //            'this error to NAIF. '   )
         call errch  ( '#', name(1:rtrim(name)) )
         call sigerr ( 'SPICE(BUG)'             )
         return

      end if

      found = .true.
      k     =  0
      n     =  0
      begin =  max( start, 1 )
      node  =  datahd

      do while ( node > 0  )

         k = k + 1

         if ( k >= begin ) then
            n = n+1
            values(n) = dpvals(     node)

            if ( n == room ) return

         end if

         node = dppool(next,node)

      end do

      return





!$Procedure GIPOOL (Get integers from the kernel pool)

      entry gipool ( name, start, room, n, ivals, found )

! $ Abstract
!
!     Return the integer value of a kernel variable from the
!     kernel pool.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     INTEGER               START
!     INTEGER               ROOM
!     INTEGER               N
!     INTEGER               IVALS    ( * )
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the variable whose value is to be returned.
!     START      I   Which component to start retrieving for NAME
!     ROOM       I   The largest number of values to return.
!     N          O   Number of values returned for NAME.
!     IVALS      O   Values associated with NAME.
!     FOUND      O   .TRUE. if variable is in pool.
!
! $ Detailed_Input
!
!     NAME     is the name of the variable whose values are to be
!              returned. If the variable is not in the pool with
!              numeric type, FOUND will be .FALSE.
!
!     START    is the index of the first component of NAME to return.
!              If START is less than 1, it will be treated as 1. If
!              START is greater than the total number of components
!              available for NAME, no values will be returned (N will
!              be set to zero). However, FOUND will still be set to
!              .TRUE.
!
!     ROOM     is the maximum number of components that should be
!              returned for this variable. (Usually it is the amount
!              of ROOM available in the array IVALS). If ROOM is
!              less than 1 the error SPICE(BADARRAYSIZE) will be
!              signaled.
!
! $ Detailed_Output
!
!     N        is the number of values associated with NAME that
!              are returned. It will always be less than or equal
!              to ROOM.
!
!              If NAME is not in the pool with numeric type, no value
!              is given to N.
!
!     IVALS    is the array of values associated with NAME. Any
!              numeric value having non-zero fractional part is
!              rounded to the closest integer. If NAME is not in the
!              pool or does not have numeric type, no values are
!              assigned to the elements of IVALS.
!
!     FOUND    is .TRUE. if the variable is in the pool and has numeric
!              type, .FALSE. if it is not.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the value of ROOM is less than one, the error
!         SPICE(BADARRAYSIZE) is signaled.
!
!     2)  If a value requested is outside the valid range
!         of integers, the error SPICE(INTOUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides the user interface for retrieving
!     integer data stored in the kernel pool. This interface
!     allows you to retrieve the data associated with a variable
!     in multiple accesses. Under some circumstances this alleviates
!     the problem of having to know in advance the maximum amount
!     of space needed to accommodate all kernel variables.
!
!     However, this method of access does come with a price. It is
!     always more efficient to retrieve all of the data associated
!     with a kernel pool data in one call than it is to retrieve
!     it in sections.
!
!     See also the entry points GDPOOL and GCPOOL.
!
! $ Examples
!
!     The following code fragment demonstrates how the data stored
!     in a kernel pool variable can be retrieved in pieces.
!
!     First we need some declarations.
!
!        INTEGER               ROOM
!        PARAMETER           ( ROOM = 3 )
!
!        CHARACTER*(8)         VARNAM
!        CHARACTER*(3)         INDENT
!        INTEGER               START
!        INTEGER               N
!        LOGICAL               FOUND
!        INTEGER               IVALS(ROOM)
!
!
!     Next load the data in the file 'typical.ker' into the
!     kernel pool.
!
!        CALL LDPOOL ( 'typical.ker' )
!
!     Next we shall print the values stored for the kernel pool
!     variable 'MYDATA'
!
!        VARNAM = 'MYDATA'
!        INDENT = ' '
!        START  =  1
!
!        CALL GIPOOL ( VARNAM, START, ROOM, N, IVALS, FOUND )
!
!        IF ( .NOT. FOUND )
!           WRITE (*,*) 'There is no numeric data available for MYDATA.'
!        ELSE
!
!           WRITE (*,*) 'Values for MYDATA.'
!           WRITE (*,*)
!
!           DO I = 1, N
!              WRITE (*,*) INDENT, IVALS(I)
!           END DO
!
!           DO WHILE ( N .EQ. ROOM )
!
!              START = START + N
!              CALL GIPOOL ( VARNAM, START, ROOM, N, IVALS, FOUND )
!
!              DO I = 1, N
!                 WRITE (*,*) INDENT, IVALS(I)
!              END DO
!
!           END DO
!
!        END IF
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
!
! $ Version
!
!-    SPICELIB Version 8.1.2, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 8.1.1, 14-JUL-2014 (NJB)
!
!        Updated description of IVALS in $Detailed_Output
!        header section.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        ZZPINI call was updated for compatibility
!        with new watcher system implementation.
!
!-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
!
!        Corrected an in-line comment relating to finding the
!        head node of the conflict resolution list for NAME.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
!        to the routine.
!
!-&


! $ Index_Entries
!
!     RETURN the integer value of a pooled kernel variable
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Perform the one obvious error check first.
!
      if ( room < 1 ) then

         call setmsg ( 'The amount of room specified as ' &
         //            'available for output in the output array ' &
         //            'was: #.  The amount of room must be ' &
         //            'positive. ' )


         call errint ( '#', room )
         call sigerr ( 'SPICE(BADARRAYSIZE)'  )
         return

      end if


!
!     Compute the hash value of this name.
!
      lookat = zzhash ( name )

!
!     Now see if there is a non-empty conflict resolution list for the
!     input string NAME.  If so, NAMLST(LOOKAT) contains the head node
!     of the conflict resolution list; this node is a positive value.
!
      if ( namlst(lookat) == 0 ) then

         found = .false.
         return

      end if
!
!     If were are still here NAMLST(LOOKAT) is the first node of
!     a conflict resolution list.  See if the NAME corresponding
!     to this node is the one we are looking for.
!
      node   = namlst( lookat      )
      succes = name == pnames(node)

      do while ( .not. succes )

         node = nmpool ( next, node )

         if ( node < 0 ) then

            found = .false.
            return

         end if

         succes = name == pnames(node)

      end do
!
!     If you get to this point, the variable NAME is present in the
!     list of names at PNAMES(NODE), ABS( DATLST(NODE) ) points to the
!     head of a linked list of values for this NAME.
!
      datahd = datlst(node)

      if ( datahd < 0 ) then

         n     =  0
         found = .false.
         return

      else if ( datahd == 0 ) then

         call setmsg ( 'This is never supposed to happen.  The ' &
         //            'requested name, ''#'', was found in the ' &
         //            'name list, but the pointer to the head ' &
         //            'of the data for this variable is zero. ' &
         //            'Please note your activities and report ' &
         //            'this error to NAIF. '   )
         call errch  ( '#', name(1:rtrim(name)) )
         call sigerr ( 'SPICE(BUG)'             )
         return

      end if
!
!     Prepare for fetching values.
!
      big   =  real( intmax(), wp )
      small =  real( intmin(), wp )
      found = .true.
      k     =  0
      n     =  0
      begin =  max( start, 1 )
      node  =  datahd

      do while ( node > 0  )

         k = k + 1

         if ( k >= begin ) then
            n = n+1

            if (       dpvals(node) >= small &
                 .and. dpvals(node) <= big   ) then

               ivals(n) = idnint( dpvals( node) )

            else

               call setmsg ( 'The value associated with index # ' &
               //            'of the kernel variable # is ' &
               //            'outside the range of integers. ' &
               //            'The value stored was: # .'      )


               call errint ( '#', k                     )
               call errch  ( '#', name(1:rtrim(name))   )
               call errdp  ( '#', dpvals(node)          )
               call sigerr ( 'SPICE(INTOUTOFRANGE)'     )
               return


            end if
            if ( n == room ) return

         end if

         node = dppool(next,node)

      end do

      return




!$Procedure DTPOOL (Data for a kernel pool variable)

      entry dtpool ( name, found, n, type )

! $ Abstract
!
!     Return the data about a kernel pool variable.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     LOGICAL               FOUND
!     INTEGER               N
!     CHARACTER*(*)         TYPE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the variable whose value is to be returned.
!     FOUND      O   .TRUE. if variable is in pool.
!     N          O   Number of values returned for NAME.
!     TYPE       O   Type of the variable 'C', 'N', 'X'
!
! $ Detailed_Input
!
!     NAME     is the name of the variable whose values are to be
!              returned.
!
! $ Detailed_Output
!
!     FOUND    is .TRUE. if the variable is in the pool .FALSE. if it
!              is not.
!
!     N        is the number of values associated with NAME.
!              If NAME is not present in the pool N will be returned
!              with the value 0.
!
!     TYPE     is the type of the variable associated with NAME.
!
!                  'C' if the data is character data
!                  'N' if the data is numeric.
!                  'X' if there is no variable NAME in the pool.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the name requested is not in the kernel pool, FOUND
!         will be set to .FALSE., N to zero and TYPE to 'X'.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows you to determine whether or not a kernel
!     pool variable is present and to determine its size and type
!     if it is.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) The following program demonstrates how to determine the
!        properties of a stored kernel variable. The program prompts
!        for text kernel name and for the name of a kernel variable.
!        If the variable is present in the kernel pool, the dimension
!        and type of the variable are displayed.
!
!
!        Example code begins here.
!
!
!              PROGRAM DTPOOL_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               RTRIM
!
!        C
!        C     Local constants
!        C
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 256 )
!
!              INTEGER               KVNMLN
!              PARAMETER           ( KVNMLN = 33 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(FILSIZ)    FNAME
!              CHARACTER*(KVNMLN)    VARNAM
!              CHARACTER*(1)         VTYPE
!
!              INTEGER               N
!
!              LOGICAL               FOUND
!
!        C
!        C     Prompt for the name of a text-kernel file.
!        C
!              CALL PROMPT ( 'Enter text-kernel name        > ', FNAME )
!
!        C
!        C     Load the kernel.
!        C
!              CALL FURNSH ( FNAME )
!
!              CALL PROMPT ( 'Enter name of kernel variable > ',
!             .               VARNAM )
!
!              CALL DTPOOL ( VARNAM, FOUND, N, VTYPE )
!
!              IF ( FOUND ) THEN
!                 WRITE(*,*) ' '
!                 WRITE(*,*) 'Properties of variable ',
!             .               VARNAM(:RTRIM(VARNAM)), ':'
!                 WRITE(*,*) ' '
!                 WRITE(*,*) '   Size:   ', N
!
!                 IF ( VTYPE .EQ. 'C' ) THEN
!
!                    WRITE(*,*) '   Type:   Character'
!
!                 ELSE
!
!                    WRITE(*,*) '   Type:   Numeric'
!                 END IF
!
!              ELSE
!
!                 WRITE(*,*) VARNAM,
!             .              ' is not present in the kernel pool.'
!
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the FK file named cas_v40.tf to ask for the
!        variable 'FRAME_-82104_NAME', the output was:
!
!
!        Enter text-kernel name        > cas_v40.tf
!        Enter name of kernel variable > FRAME_-82104_NAME
!
!         Properties of variable FRAME_-82104_NAME:
!
!            Size:              1
!            Type:   Character
!
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
!
! $ Version
!
!-    SPICELIB Version 8.1.1, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        ZZPINI call was updated for compatibility
!        with new watcher system implementation.
!
!-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
!
!        Corrected an in-line comment relating to finding the
!        head node of the conflict resolution list for NAME.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!
!        The implementation of the kernel pool was completely redone
!        to improve performance in loading and fetching data. In
!        addition the pool was upgraded so that variables may be
!        either string or numeric valued.
!
!        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
!        to the routine.
!
!-&


! $ Index_Entries
!
!     RETURN summary information about a kernel pool variable
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Until we find otherwise, we shall assume there is no data
!     for this variable.
!
      found = .false.
      n     =  0
      type  = 'X'

!
!     Compute the hash value of this name.
!
      lookat = zzhash ( name )

!
!     Now see if there is a non-empty conflict resolution list for the
!     input string NAME.  If so, NAMLST(LOOKAT) contains the head node
!     of the conflict resolution list; this node is a positive value.
!
      if ( namlst(lookat) == 0 ) return
!
!     If were are still here NAMLST(LOOKAT) is the first node of
!     a conflict resolution list.  See if the NAME corresponding
!     to this node is the one we are looking for.
!
      node   = namlst( lookat      )
      succes = name == pnames(node)

      do while ( .not. succes )

         node = nmpool ( next, node )

         if ( node < 0 ) return

         succes = name == pnames(node)

      end do
!
!     If you get to this point, the variable NAME is present in the
!     list of names at PNAMES(NODE), ABS( DATLST(NODE) ) points to the
!     head of a linked list of values for this NAME.
!

      datahd = datlst(node)

      if ( datahd < 0 ) then

         type  = 'C'
         found = .true.
         node  = -datahd

         do while ( node > 0 )
            n    = n + 1
            node = chpool(next,node)
         end do

      else if ( datahd > 0 ) then

         type  = 'N'
         found = .true.
         node  =  datahd

         do while ( node > 0 )
            n    = n + 1
            node = dppool(next,node)
         end do

      else if ( datahd == 0 ) then

         call setmsg ( 'This is never supposed to happen.  The ' &
         //            'requested name, ''#'', was found in the ' &
         //            'name list, but the pointer to the head ' &
         //            'of the data for this variable is zero. ' &
         //            'Please note your activities and report ' &
         //            'this error to NAIF. '   )
         call errch  ( '#', name(1:rtrim(name)) )
         call sigerr ( 'SPICE(BUG)'             )

         return

      end if

      return




!$Procedure PCPOOL ( Put character strings into the kernel pool )

      entry pcpool ( name, n, cvals )

! $ Abstract
!
!     Provide toolkit programmers a method for programmatically
!     inserting character data into the kernel pool.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     POOL
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     INTEGER               N
!     CHARACTER*(*)         CVALS ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   The kernel pool name to associate with CVALS.
!     N          I   The number of values to insert.
!     CVALS      I   An array of strings to insert into the kernel pool.
!
! $ Detailed_Input
!
!     NAME     is the name of the kernel pool variable to associate
!              with the values supplied in the array CVALS
!
!     N        is the number of values to insert into the kernel pool.
!
!     CVALS    is an array of strings to insert into the kernel
!              pool.
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
!     1)  If NAME is already present in the kernel pool and there
!         is sufficient room to hold all values supplied in CVALS,
!         the old values associated with NAME will be overwritten.
!
!     2)  If there is not sufficient room to insert a new variable into
!         the kernel pool and NAME is not already present in the kernel
!         pool, an error is signaled by a routine in the call tree of
!         this routine.
!
!     3)  If there is not sufficient room to insert the values
!         associated with NAME, the error SPICE(NOMOREROOM) is signaled.
!
!     4)  If the kernel pool variable name length exceeds its maximum
!         allowed length (see Kernel Required Reading, kernel.req), the
!         error SPICE(BADVARNAME) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point provides a programmatic interface for inserting
!     character data into the SPICE kernel pool without reading an
!     external file.
!
! $ Examples
!
!     Suppose that you wish to supply default values for a program
!     so that it may function even in the absence of the appropriate
!     text kernels. You can use the entry points PCPOOL, PDPOOL
!     and PIPOOL to initialize the kernel pool with suitable
!     values at program initialization. The example below shows
!     how you might set up various kernel pool variables that might
!     be required by a program.
!
!
!        Set up the relationship between the EARTH_BODYFIXED frame
!        and the IAU_EARTH frame.
!
!        CALL IDENT  ( MATRIX )
!        CALL PCPOOL ( 'TKFRAME_EARTH_FIXED_SPEC',     1, 'MATRIX'    )
!        CALL PCPOOL ( 'TKFRAME_EARTH_FIXED_RELATIVE', 1, 'IAU_EARTH' )
!        CALL PDPOOL ( 'TKFRAME_EARTH_FIXED_MATRIX',   9,  MATRIX )
!
!
!        Load the IAU model for the earth's rotation and shape.
!
!
!        RA ( 1 ) =  0.0D0
!        RA ( 2 ) = -0.641D0
!        RA ( 3 ) =  0.0D0
!
!        DEC( 1 ) = 90.0D0
!        DEC( 2 ) = -0.557D0
!        DEC( 3 ) =  0.0D0
!
!        PM ( 1 ) = 190.16D0
!        PM ( 2 ) = 360.9856235D0
!        PM ( 3 ) =   0.0D0
!
!        R  ( 1 ) =  6378.140D0
!        R  ( 2 ) =  6378.140D0
!        R  ( 3 ) =  6356.75D0
!
!        CALL PDPOOL ( 'BODY399_POLE_RA',   3, RA  )
!        CALL PDPOOL ( 'BODY399_POLE_DEC',  3, DEC )
!        CALL PDPOOL ( 'BODY399_PM',        3, PM  )
!        CALL PDPOOL ( 'BODY399_RADII',     3, R   )
!
!
!        Set up a preliminary set of leapsecond values.
!
!        CALL PDPOOL ( 'DELTET/DELTA_T_A/',1, 32.184D0  )
!        CALL PDPOOL ( 'DELTET/K',         1,  1.657D-3 )
!        CALL PDPOOL ( 'DELTET/EB',        1,  1.671D-2 )
!
!        VALUES(1) = 6.23999600D0
!        VALUES(2) = 1.99096871D-7
!
!        CALL PDPOOL ( 'DELTET/M', 2, VALUES )
!
!
!        VALUES(  1 ) = 10
!        VALUES(  3 ) = 11
!        VALUES(  5 ) = 12
!        VALUES(  7 ) = 13
!        VALUES(  9 ) = 14
!        VALUES( 11 ) = 15
!        VALUES( 13 ) = 16
!        VALUES( 15 ) = 17
!        VALUES( 17 ) = 18
!        VALUES( 19 ) = 19
!        VALUES( 21 ) = 20
!        VALUES( 23 ) = 21
!        VALUES( 25 ) = 22
!        VALUES( 27 ) = 23
!        VALUES( 29 ) = 24
!        VALUES( 31 ) = 25
!        VALUES( 33 ) = 26
!        VALUES( 35 ) = 27
!        VALUES( 37 ) = 28
!        VALUES( 39 ) = 29
!        VALUES( 41 ) = 30
!        VALUES( 43 ) = 31
!
!        CALL TPARSE ( '1972-JAN-1', VALUES(2),  ERROR )
!        CALL TPARSE ( '1972-JUL-1', VALUES(4),  ERROR )
!        CALL TPARSE ( '1973-JAN-1', VALUES(6),  ERROR )
!        CALL TPARSE ( '1974-JAN-1', VALUES(8),  ERROR )
!        CALL TPARSE ( '1975-JAN-1', VALUES(10), ERROR )
!        CALL TPARSE ( '1976-JAN-1', VALUES(12), ERROR )
!        CALL TPARSE ( '1977-JAN-1', VALUES(14), ERROR )
!        CALL TPARSE ( '1978-JAN-1', VALUES(16), ERROR )
!        CALL TPARSE ( '1979-JAN-1', VALUES(18), ERROR )
!        CALL TPARSE ( '1980-JAN-1', VALUES(20), ERROR )
!        CALL TPARSE ( '1981-JUL-1', VALUES(22), ERROR )
!        CALL TPARSE ( '1982-JUL-1', VALUES(24), ERROR )
!        CALL TPARSE ( '1983-JUL-1', VALUES(26), ERROR )
!        CALL TPARSE ( '1985-JUL-1', VALUES(28), ERROR )
!        CALL TPARSE ( '1988-JAN-1', VALUES(30), ERROR )
!        CALL TPARSE ( '1990-JAN-1', VALUES(32), ERROR )
!        CALL TPARSE ( '1991-JAN-1', VALUES(34), ERROR )
!        CALL TPARSE ( '1992-JUL-1', VALUES(36), ERROR )
!        CALL TPARSE ( '1993-JUL-1', VALUES(38), ERROR )
!        CALL TPARSE ( '1994-JUL-1', VALUES(40), ERROR )
!        CALL TPARSE ( '1996-JAN-1', VALUES(42), ERROR )
!        CALL TPARSE ( '1997-JUL-1', VALUES(44), ERROR )
!
!        CALL PDPOOL ( 'DELTET/DELTA_AT',  44, VALUES )
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 9.1.1, 27-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 9.1.0, 17-JAN-2014 (BVS) (NJB)
!
!        Updated to increment POOL state counter.
!        Updated $Index_Entries section.
!
!-    SPICELIB Version 9.0.0, 24-MAY-2010 (EDW)
!
!        Added an error check on the length of the kernel pool variable
!        name argument to enforce the variable name length does not
!        exceed MAXLEN.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        Watcher update code was re-written for compatibility
!        with new watcher system implementation.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory instead
!        of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-&


! $ Index_Entries
!
!     Set the value of a character_variable in the kernel_pool
!
!-&


!
!     Standard SPICE error handling.
!
      if ( n <= 0 ) return

      if ( return() ) return



!
!     Check the variable name length; signal an error
!     if longer than MAXLEN.
!
      varlen = len( name(1: lastnb(name) )  )

      if ( varlen > maxlen ) then

         call setmsg ( 'The input kernel pool variable name ' &
         //            'exceeds the maximum allowed ' &
         //            'length of #1. The length of the variable ' &
         //            'name is #2, the offending variable name: ' &
         //            '''#3''.')

         call errint ( '#1', maxlen                   )
         call errint ( '#2', varlen                   )
         call errch  ( '#3', name                     )
         call sigerr ( 'SPICE(BADVARNAME)'            )
         return

      end if

!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Increment POOL state counter.
!
      call zzctrinc ( subctr )

!
!     Find out where the name for this item is located
!     in the data tables.
!
      call zzgpnm ( namlst, &
                    nmpool, pnames, datlst, &
                    dppool, dpvals, &
                    chpool, chvals, &
                    name,   gotit,  lookat, nameat )

      if ( failed() ) return
!
!     Determine how much room is available for inserting new d.p.s
!     values into the kernel pool.
!
      avail = lnknfn(chpool)

      if ( gotit ) then
!
!        If we found the specified variable in the kernel pool, we
!        may be able to free up some space before inserting data.
!        We need to take this into account when determining
!        the amount of free room in the pool.
!
         datahd         = datlst( nameat )

         if ( datahd > 0 ) then
!
!           No extra strings will be freed.  We have whatever
!           free space is in the CHPOOL right now.
!
         else
!
!           Find out how many items are in the current
!           list of strings associated with the variable.
!
            tofree =   0
            node   = - datahd

            do while ( node > 0 )
               tofree = tofree + 1
               node   = chpool(next,node)
            end do
!
!           Add the number we will free to the amount currently
!           free in the dp pool.
!
            avail = avail + tofree

         end if

      end if

!
!     If the AVAIL for new data is less than the number of items
!     to be added, we just bail out here.
!
      if ( avail < n ) then

         if ( .not. gotit ) then
!
!           We need to perform some clean up.  We've allocated
!           a new name but it has nothing in it. On the other hand
!           if we found it don't need to do anything because we've
!           only read from the pool. We haven't altered anything.
!           But in that case we'll never get into this block of code.
!
            call zzcln ( lookat, nameat, &
                         namlst, datlst, nmpool, chpool, dppool )

         end if


         call setmsg ( 'There is not sufficient space available ' &
         //            'in the kernel pool to store the # items ' &
         //            'associated with the name #.  There is ' &
         //            'room to store only # items. ' )
         call errint ( '#', n  )
         call errch  ( '#', name )
         call errint ( '#', avail   )
         call sigerr ( 'SPICE(NOMOREROOM)' )
         return

      end if

!
!     There is room to insert the data.  Free up any required
!     nodes.
!
      if ( gotit ) then

!
!        We need to free the data associated with this
!        variable.  But first make sure there will be room
!        to add data.
!
         datahd         = datlst( nameat )
         datlst(nameat) = 0

         if ( datahd > 0 ) then

!
!           This variable was character type we need to
!           free a linked list from the character data
!           pool.
!
            head =   datahd
            tail = - dppool(prev,head)

            call lnkfsl ( head, tail, dppool )

         else

!
!           This variable was character type. We need to
!           free a linked list from the numeric pool.
!
            head = - datahd
            tail = - chpool(prev,head)

            call lnkfsl ( head, tail, chpool )

         end if

      end if
!
!     We have done all of the freeing and checking that
!     needs to be done.  Now add the data.
!
      do i = 1, n

!
!        We are ready to go.  Allocate a node for this data
!        item. First make sure there is room to do so.
!
         free = lnknfn ( chpool )

         if ( free <= 0 ) then

            call setmsg ( 'There is no room available for ' &
            //            'adding another character value ' &
            //            'to the kernel pool.' )
            call sigerr ( 'SPICE(KERNELPOOLFULL)' )
            return

         end if

!
!        Allocate a node for storing this string value:
!
         call lnkan ( chpool, chnode )

         if ( datlst(nameat) == 0 ) then

!
!           There was no data for this name yet.  We make
!           CHNODE be the head of the data list for this name.
!
            datlst(nameat) = -chnode

         else

!
!           Put this node after the tail of the current list.
!
            head = -datlst(nameat)
            tail = -chpool(prev, head )

            call lnkila ( tail, chnode, chpool )

         end if

!
!        Finally insert this data item in the data buffer
!        at CHNODE.  Note any quotes will be doubled so we
!        have to undo this affect when we store the data.
!
         chvals(chnode) = cvals(i)

!
!        That's all for this value. It's now time to loop
!        back through and get the next value.
!

      end do


!
!     One last thing, see if this variable is being watched,
!     If it is, add its associated agents to the list of
!     AGENTS to be notified of a watched variable update.
!
      if (  elemc( name, wtvars )  ) then
!
!        Union the update set AGENTS with the set of agents
!        associated with the variable NAME.
!
         call zznwpool ( name,   wtvars, wtptrs, wtpool, &
                         wtagnt, active, notify, agents )

      end if

      return




!$Procedure PDPOOL ( Put d.p.'s into the kernel pool )

      entry pdpool ( name, n, values )

! $ Abstract
!
!     Provide toolkit programmers a method for programmatically
!     inserting double precision data into the kernel pool.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     POOL
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     INTEGER               N
!     DOUBLE PRECISION      VALUES ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   The kernel pool name to associate with VALUES.
!     N          I   The number of values to insert.
!     VALUES     I   An array of values to insert into the kernel pool.
!
! $ Detailed_Input
!
!     NAME     is the name of the kernel pool variable to associate
!              with the values supplied in the array VALUES
!
!     N        is the number of values to insert into the kernel pool.
!
!     VALUES   is an array of d.p. values to insert into the kernel
!              pool.
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
!     1)  If NAME is already present in the kernel pool and there
!         is sufficient room to hold all values supplied in VALUES,
!         the old values associated with NAME will be overwritten.
!
!     2)  If there is not sufficient room to insert a new variable into
!         the kernel pool and NAME is not already present in the kernel
!         pool, an error is signaled by a routine in the call tree of
!         this routine.
!
!     3)  If there is not sufficient room to insert the values
!         associated with NAME, the error SPICE(NOMOREROOM) is signaled.
!
!     4)  If the kernel pool variable name length exceeds its maximum
!         allowed length (see Kernel Required Reading, kernel.req), the
!         error SPICE(BADVARNAME) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point provides a programmatic interface for inserting
!     data into the SPICE kernel pool without reading an external file.
!
! $ Examples
!
!     Suppose that you wish to supply default values for a program
!     so that it may function even in the absence of the appropriate
!     text kernels. You can use the entry points PCPOOL, PDPOOL
!     and PIPOOL to initialize the kernel pool with suitable
!     values at program initialization. The example below shows
!     how you might set up various kernel pool variables that might
!     be required by a program.
!
!
!        Set up the relationship between the EARTH_BODYFIXED frame
!        and the IAU_EARTH frame.
!
!        CALL IDENT  ( MATRIX )
!        CALL PCPOOL ( 'TKFRAME_EARTH_FIXED_SPEC',     1, 'MATRIX'    )
!        CALL PCPOOL ( 'TKFRAME_EARTH_FIXED_RELATIVE', 1, 'IAU_EARTH' )
!        CALL PDPOOL ( 'TKFRAME_EARTH_FIXED_MATRIX',   9,  MATRIX )
!
!
!        Load the IAU model for the earth's rotation and shape.
!
!
!        RA ( 1 ) =  0.0D0
!        RA ( 2 ) = -0.641D0
!        RA ( 3 ) =  0.0D0
!
!        DEC( 1 ) = 90.0D0
!        DEC( 2 ) = -0.557D0
!        DEC( 3 ) =  0.0D0
!
!        PM ( 1 ) = 190.16D0
!        PM ( 2 ) = 360.9856235D0
!        PM ( 3 ) =   0.0D0
!
!        R  ( 1 ) =  6378.140D0
!        R  ( 2 ) =  6378.140D0
!        R  ( 3 ) =  6356.75D0
!
!        CALL PDPOOL ( 'BODY399_POLE_RA',   3, RA  )
!        CALL PDPOOL ( 'BODY399_POLE_DEC',  3, DEC )
!        CALL PDPOOL ( 'BODY399_PM',        3, PM  )
!        CALL PDPOOL ( 'BODY399_RADII',     3, R   )
!
!
!        Set up a preliminary set of leapsecond values.
!
!        CALL PDPOOL ( 'DELTET/DELTA_T_A', 1, 32.184D0  )
!        CALL PDPOOL ( 'DELTET/K',         1,  1.657D-3 )
!        CALL PDPOOL ( 'DELTET/EB',        1,  1.671D-2 )
!
!        VALUES(1) = 6.23999600D0
!        VALUES(2) = 1.99096871D-7
!
!        CALL PDPOOL ( 'DELTET/M', 2, VALUES )
!
!
!        VALUES(  1 ) = 10
!        VALUES(  3 ) = 11
!        VALUES(  5 ) = 12
!        VALUES(  7 ) = 13
!        VALUES(  9 ) = 14
!        VALUES( 11 ) = 15
!        VALUES( 13 ) = 16
!        VALUES( 15 ) = 17
!        VALUES( 17 ) = 18
!        VALUES( 19 ) = 19
!        VALUES( 21 ) = 20
!        VALUES( 23 ) = 21
!        VALUES( 25 ) = 22
!        VALUES( 27 ) = 23
!        VALUES( 29 ) = 24
!        VALUES( 31 ) = 25
!        VALUES( 33 ) = 26
!        VALUES( 35 ) = 27
!        VALUES( 37 ) = 28
!        VALUES( 39 ) = 29
!        VALUES( 41 ) = 30
!        VALUES( 43 ) = 31
!
!        CALL TPARSE ( '1972-JAN-1', VALUES(2),  ERROR )
!        CALL TPARSE ( '1972-JUL-1', VALUES(4),  ERROR )
!        CALL TPARSE ( '1973-JAN-1', VALUES(6),  ERROR )
!        CALL TPARSE ( '1974-JAN-1', VALUES(8),  ERROR )
!        CALL TPARSE ( '1975-JAN-1', VALUES(10), ERROR )
!        CALL TPARSE ( '1976-JAN-1', VALUES(12), ERROR )
!        CALL TPARSE ( '1977-JAN-1', VALUES(14), ERROR )
!        CALL TPARSE ( '1978-JAN-1', VALUES(16), ERROR )
!        CALL TPARSE ( '1979-JAN-1', VALUES(18), ERROR )
!        CALL TPARSE ( '1980-JAN-1', VALUES(20), ERROR )
!        CALL TPARSE ( '1981-JUL-1', VALUES(22), ERROR )
!        CALL TPARSE ( '1982-JUL-1', VALUES(24), ERROR )
!        CALL TPARSE ( '1983-JUL-1', VALUES(26), ERROR )
!        CALL TPARSE ( '1985-JUL-1', VALUES(28), ERROR )
!        CALL TPARSE ( '1988-JAN-1', VALUES(30), ERROR )
!        CALL TPARSE ( '1990-JAN-1', VALUES(32), ERROR )
!        CALL TPARSE ( '1991-JAN-1', VALUES(34), ERROR )
!        CALL TPARSE ( '1992-JUL-1', VALUES(36), ERROR )
!        CALL TPARSE ( '1993-JUL-1', VALUES(38), ERROR )
!        CALL TPARSE ( '1994-JUL-1', VALUES(40), ERROR )
!        CALL TPARSE ( '1996-JAN-1', VALUES(42), ERROR )
!        CALL TPARSE ( '1997-JUL-1', VALUES(44), ERROR )
!
!        CALL PDPOOL ( 'DELTET/DELTA_AT',  44, VALUES )
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 9.1.1, 27-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 9.1.0, 17-JAN-2014 (BVS) (NJB)
!
!        Updated to increment POOL state counter.
!        Updated $Index_Entries section.
!
!-    SPICELIB Version 9.0.0, 24-MAY-2010 (EDW)
!
!        Added an error check on the length of the kernel pool variable
!        name argument to enforce the variable name length does not
!        exceed MAXLEN.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        Watcher update code was re-written for compatibility
!        with new watcher system implementation.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory instead
!        of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-&


! $ Index_Entries
!
!     Set the value of a d.p._variable in the kernel_pool
!
!-&


!
!     Standard SPICE error handling.
!
      if ( n <= 0 ) return

      if ( return() ) return



!
!     Check the variable name length; signal an error
!     if longer than MAXLEN.
!
      varlen = len( name(1: lastnb(name) )  )

      if ( varlen > maxlen ) then

         call setmsg ( 'The input kernel pool variable name ' &
         //            'exceeds the maximum allowed ' &
         //            'length of #1. The length of the variable ' &
         //            'name is #2, the offending variable name: ' &
         //            '''#3''.')

         call errint ( '#1', maxlen                   )
         call errint ( '#2', varlen                   )
         call errch  ( '#3', name                     )
         call sigerr ( 'SPICE(BADVARNAME)'            )
         return

      end if

!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Increment POOL state counter.
!
      call zzctrinc ( subctr )

!
!     Find out where the name for this item is located
!     in the data tables.
!
      call zzgpnm ( namlst, &
                    nmpool, pnames, datlst, &
                    dppool, dpvals, &
                    chpool, chvals, &
                    name,   gotit,  lookat, nameat )

      if ( failed() ) return
!
!     Determine how much room is available for inserting new d.p.s
!     values into the kernel pool.
!
      avail = lnknfn(dppool)

      if ( gotit ) then
!
!        If we found the specified variable in the kernel pool, we
!        may be able to free up some space before inserting data.
!        We need to take this into account when determining
!        the amount of free room in the pool.
!
         datahd         = datlst( nameat )

         if ( datahd < 0 ) then
!
!           No extra d.p.s will be freed.  We have whatever
!           free space is in the DPPOOL right now.
!
         else
!
!           Find out how many items are in the current
!           list of d.p. associated with the variable.
!
            tofree = 0
            node   =  datahd

            do while ( node > 0 )
               tofree = tofree + 1
               node   = dppool(next,node)
            end do
!
!           Add the number we will free to the amount currently
!           free in the dp pool.
!
            avail = avail + tofree

         end if

      end if

!
!     If the AVAIL for new data is less than the number of items
!     to be added, we just bail out here.
!
      if ( avail < n ) then

         if ( .not. gotit ) then
!
!           We need to perform some clean up.  We've allocated
!           a new name but it has nothing in it. On the other hand
!           if we found it don't need to do anything because we've
!           only read from the pool. We haven't altered anything.
!           But in that case we'll never get into this block of code.
!
            call zzcln ( lookat, nameat, &
                         namlst, datlst, nmpool, chpool, dppool )

         end if


         call setmsg ( 'There is not sufficient space available ' &
         //            'in the kernel pool to store the # items ' &
         //            'associated with the name #.  There is ' &
         //            'room to store only # items. ' )
         call errint ( '#', n  )
         call errch  ( '#', name )
         call errint ( '#', avail   )
         call sigerr ( 'SPICE(NOMOREROOM)' )
         return

      end if

!
!     There is room to insert the data.  Free up any required
!     nodes.
!
      if ( gotit ) then

!
!        We need to free the data associated with this
!        variable.  But first make sure there will be room
!        to add data.
!
         datahd         = datlst( nameat )
         datlst(nameat) = 0

         if ( datahd < 0 ) then

!
!           This variable was character type we need to
!           free a linked list from the character data
!           pool.
!
            head = - datahd
            tail = - chpool(prev,head)

            call lnkfsl ( head, tail, chpool )

         else

!
!           This variable was numeric type. We need to
!           free a linked list from the numeric pool.
!
            head =   datahd
            tail = - dppool(prev,head)

            call lnkfsl ( head, tail, dppool )

         end if

      end if
!
!     We have done all of the freeing and checking that
!     needs to be done.  Now add the data.
!
      do i = 1, n

!
!        OK. See if there is room in
!        the numeric portion of the pool to store this value.
!
         free = lnknfn ( dppool )

         if ( free <= 0 ) then
!
!           This branch of the code should never be exercised,
!           but it doesn't hurt to program in a redundant check.
!
            call zzcln ( lookat, nameat, &
                         namlst, datlst, nmpool, chpool, dppool )

            call setmsg ( 'There is no room available for ' &
            //            'adding another numeric value ' &
            //            'to the kernel pool.' )
            call sigerr ( 'SPICE(KERNELPOOLFULL)' )
            return

         end if

!
!        Allocate a node for storing this numeric value:
!
         call lnkan ( dppool, dpnode )

         if ( datlst(nameat) == 0 ) then

!
!           There was no data for this name yet.  We make
!           DPNODE be the head of the data list for this name.
!
            datlst(nameat) =  dpnode

         else

!
!           Put this node after the tail of the current list.
!
            head =  datlst(nameat)
            tail = -dppool(prev, head )

            call lnkila ( tail, dpnode, dppool )

         end if

!
!        Finally insert this data item into the numeric buffer.
!
         dpvals(dpnode) = values(i)

      end do


!
!     One last thing, see if this variable is being watched,
!     If it is, add its associated agents to the list of
!     AGENTS to be notified of a watched variable update.
!
      if (  elemc( name, wtvars )  ) then
!
!        Union the update set AGENTS with the set of agents
!        associated with the variable NAME.
!
         call zznwpool ( name,   wtvars, wtptrs, wtpool, &
                         wtagnt, active, notify, agents )
      end if


      return




!$Procedure PIPOOL ( Put integers into the kernel pool )

      entry pipool ( name, n, ivals )

! $ Abstract
!
!     Provide toolkit programmers a method for programmatically
!     inserting integer data into the kernel pool.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     POOL
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     INTEGER               N
!     INTEGER               IVALS ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   The kernel pool name to associate with IVALS.
!     N          I   The number of values to insert.
!     IVALS      I   An array of integers to insert into the pool.
!
! $ Detailed_Input
!
!     NAME     is the name of the kernel pool variable to associate
!              with the values supplied in the array IVALS
!
!     N        is the number of values to insert into the kernel pool.
!
!     IVALS    is an array of integers to insert into the kernel
!              pool.
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
!     1)  If NAME is already present in the kernel pool and there
!         is sufficient room to hold all values supplied in IVALS,
!         the old values associated with NAME will be overwritten.
!
!     2)  If there is not sufficient room to insert a new variable into
!         the kernel pool and NAME is not already present in the kernel
!         pool, an error is signaled by a routine in the call tree of
!         this routine.
!
!     3)  If there is not sufficient room to insert the values
!         associated with NAME, the error SPICE(NOMOREROOM) is signaled.
!
!     4)  If the kernel pool variable name length exceeds its maximum
!         allowed length (see Kernel Required Reading, kernel.req), the
!         error SPICE(BADVARNAME) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point provides a programmatic interface for inserting
!     data into the SPICE kernel pool without reading an external file.
!
! $ Examples
!
!     Suppose that you wish to supply default values for a program
!     so that it may function even in the absence of the appropriate
!     text kernels. You can use the entry points PCPOOL, PDPOOL
!     and PIPOOL to initialize the kernel pool with suitable
!     values at program initialization. The example below shows
!     how you might set up various kernel pool variables that might
!     be required by a program.
!
!
!        Set up the relationship between the EARTH_BODYFIXED frame
!        and the IAU_EARTH frame.
!
!        CALL IDENT ( MATRIX )
!        CALL PCPOOL ( 'TKFRAME_EARTH_FIXED_SPEC',     1, 'MATRIX' )
!        CALL PIPOOL ( 'TKFRAME_EARTH_FIXED_RELATIVE', 1,  10081   )
!        CALL PDPOOL ( 'TKFRAME_EARTH_FIXED_MATRIX',   9,  MATRIX  )
!
!
!        Load the IAU model for the earth's rotation and shape.
!
!
!        RA ( 1 ) =  0.0D0
!        RA ( 2 ) = -0.641D0
!        RA ( 3 ) =  0.0D0
!
!        DEC( 1 ) = 90.0D0
!        DEC( 2 ) = -0.557D0
!        DEC( 3 ) =  0.0D0
!
!        PM ( 1 ) = 190.16D0
!        PM ( 2 ) = 360.9856235D0
!        PM ( 3 ) =   0.0D0
!
!        R  ( 1 ) =  6378.140D0
!        R  ( 2 ) =  6378.140D0
!        R  ( 3 ) =  6356.75D0
!
!        CALL PDPOOL ( 'BODY399_POLE_RA',   3, RA  )
!        CALL PDPOOL ( 'BODY399_POLE_DEC',  3, DEC )
!        CALL PDPOOL ( 'BODY399_PM',        3, PM  )
!        CALL PDPOOL ( 'BODY399_RADII',     3, R   )
!
!
!        Set up a preliminary set of leapsecond values.
!
!        CALL PDPOOL ( 'DELTET/DELTA_T_A/',1, 32.184D0  )
!        CALL PDPOOL ( 'DELTET/K',         1,  1.657D-3 )
!        CALL PDPOOL ( 'DELTET/EB',        1,  1.671D-2 )
!
!        VALUES(1) = 6.23999600D0
!        VALUES(2) = 1.99096871D-7
!
!        CALL PDPOOL ( 'DELTET/M', 2, VALUES )
!
!
!        VALUES(  1 ) = 10
!        VALUES(  3 ) = 11
!        VALUES(  5 ) = 12
!        VALUES(  7 ) = 13
!        VALUES(  9 ) = 14
!        VALUES( 11 ) = 15
!        VALUES( 13 ) = 16
!        VALUES( 15 ) = 17
!        VALUES( 17 ) = 18
!        VALUES( 19 ) = 19
!        VALUES( 21 ) = 20
!        VALUES( 23 ) = 21
!        VALUES( 25 ) = 22
!        VALUES( 27 ) = 23
!        VALUES( 29 ) = 24
!        VALUES( 31 ) = 25
!        VALUES( 33 ) = 26
!        VALUES( 35 ) = 27
!        VALUES( 37 ) = 28
!        VALUES( 39 ) = 29
!        VALUES( 41 ) = 30
!        VALUES( 43 ) = 31
!
!        CALL TPARSE ( '1972-JAN-1', VALUES(2),  ERROR )
!        CALL TPARSE ( '1972-JUL-1', VALUES(4),  ERROR )
!        CALL TPARSE ( '1973-JAN-1', VALUES(6),  ERROR )
!        CALL TPARSE ( '1974-JAN-1', VALUES(8),  ERROR )
!        CALL TPARSE ( '1975-JAN-1', VALUES(10), ERROR )
!        CALL TPARSE ( '1976-JAN-1', VALUES(12), ERROR )
!        CALL TPARSE ( '1977-JAN-1', VALUES(14), ERROR )
!        CALL TPARSE ( '1978-JAN-1', VALUES(16), ERROR )
!        CALL TPARSE ( '1979-JAN-1', VALUES(18), ERROR )
!        CALL TPARSE ( '1980-JAN-1', VALUES(20), ERROR )
!        CALL TPARSE ( '1981-JUL-1', VALUES(22), ERROR )
!        CALL TPARSE ( '1982-JUL-1', VALUES(24), ERROR )
!        CALL TPARSE ( '1983-JUL-1', VALUES(26), ERROR )
!        CALL TPARSE ( '1985-JUL-1', VALUES(28), ERROR )
!        CALL TPARSE ( '1988-JAN-1', VALUES(30), ERROR )
!        CALL TPARSE ( '1990-JAN-1', VALUES(32), ERROR )
!        CALL TPARSE ( '1991-JAN-1', VALUES(34), ERROR )
!        CALL TPARSE ( '1992-JUL-1', VALUES(36), ERROR )
!        CALL TPARSE ( '1993-JUL-1', VALUES(38), ERROR )
!        CALL TPARSE ( '1994-JUL-1', VALUES(40), ERROR )
!        CALL TPARSE ( '1996-JAN-1', VALUES(42), ERROR )
!        CALL TPARSE ( '1997-JUL-1', VALUES(44), ERROR )
!
!        CALL PDPOOL ( 'DELTET/DELTA_AT',  44, VALUES )
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 9.1.1, 27-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 9.1.0, 17-JAN-2014 (BVS) (NJB)
!
!        Updated to increment POOL state counter.
!        Updated $Index_Entries section.
!
!-    SPICELIB Version 9.0.0, 24-MAY-2010 (EDW)
!
!        Added an error check on the length of the kernel pool variable
!        name argument to enforce the variable name length does not
!        exceed MAXLEN.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        Watcher update code was re-written for compatibility
!        with new watcher system implementation.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory instead
!        of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-&


! $ Index_Entries
!
!     Set the value of an integer_variable in the kernel_pool
!
!-&


!
!     Standard SPICE error handling.
!
      if ( n <= 0 ) return

      if ( return() ) return



!
!     Check the variable name length; signal an error
!     if longer than MAXLEN.
!
      varlen = len( name(1: lastnb(name) )  )

      if ( varlen > maxlen ) then

         call setmsg ( 'The input kernel pool variable name ' &
         //            'exceeds the maximum allowed ' &
         //            'length of #1. The length of the variable ' &
         //            'name is #2, the offending variable name: ' &
         //            '''#3''.')

         call errint ( '#1', maxlen                   )
         call errint ( '#2', varlen                   )
         call errch  ( '#3', name                     )
         call sigerr ( 'SPICE(BADVARNAME)'            )
         return

      end if

!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Increment POOL state counter.
!
      call zzctrinc ( subctr )

!
!     Find out where the name for this item is located
!     in the data tables.
!
      call zzgpnm ( namlst, &
                    nmpool, pnames, datlst, &
                    dppool, dpvals, &
                    chpool, chvals, &
                    name, gotit,  lookat, nameat )

      if ( failed() ) return
!
!     Determine how much room is available for inserting new d.p.s
!     values into the kernel pool.
!
      avail = lnknfn(dppool)

      if ( gotit ) then
!
!        If we found the specified variable in the kernel pool, we
!        may be able to free up some space before inserting data.
!        We need to take this into account when determining
!        the amount of free room in the pool.
!
         datahd         = datlst( nameat )

         if ( datahd < 0 ) then
!
!           No extra d.p.s will be freed.  We have whatever
!           free space is in the DPPOOL right now.
!
         else
!
!           Find out how many items are in the current
!           list of d.p. associated with the variable.
!
            tofree = 0
            node   =  datahd

            do while ( node > 0 )
               tofree = tofree + 1
               node   = dppool(next,node)
            end do
!
!           Add the number we will free to the amount currently
!           free in the dp pool.
!
            avail = avail + tofree

         end if

      end if

!
!     If the AVAIL for new data is less than the number of items
!     to be added, we just bail out here.
!
      if ( avail < n ) then

         if ( .not. gotit ) then
!
!           We need to perform some clean up.  We've allocated
!           a new name but it has nothing in it. On the other hand
!           if we found it don't need to do anything because we've
!           only read from the pool. We haven't altered anything.
!           But in that case we'll never get into this block of code.
!
            call zzcln ( lookat, nameat, &
                         namlst, datlst, nmpool, chpool, dppool )

         end if

         call setmsg ( 'There is not sufficient space available ' &
         //            'in the kernel pool to store the # items ' &
         //            'associated with the name #.  There is ' &
         //            'room to store only # items. ' )
         call errint ( '#', n  )
         call errch  ( '#', name )
         call errint ( '#', avail   )
         call sigerr ( 'SPICE(NOMOREROOM)' )
         return

      end if

!
!     There is room to insert the data.  Free up any required
!     nodes.
!
      if ( gotit ) then

!
!        We need to free the data associated with this
!        variable.  But first make sure there will be room
!        to add data.
!
         datahd         = datlst( nameat )
         datlst(nameat) = 0

         if ( datahd < 0 ) then

!
!           This variable was character type we need to
!           free a linked list from the character data
!           pool.
!
            head = - datahd
            tail = - chpool(prev,head)

            call lnkfsl ( head, tail, chpool )

         else

!
!           This variable was numeric type. We need to
!           free a linked list from the numeric pool.
!
            head =   datahd
            tail = - dppool(prev,head)

            call lnkfsl ( head, tail, dppool )

         end if

      end if
!
!     We have done all of the freeing and checking that
!     needs to be done.  Now add the data.
!
      do i = 1, n

!
!        OK. See if there is room in
!        the numeric portion of the pool to store this value.
!
         free = lnknfn ( dppool )

         if ( free <= 0 ) then
!
!           This branch of the code should never be exercised,
!           but it doesn't hurt to program in a redundant check.
!
            call zzcln ( lookat, nameat, &
                         namlst, datlst, nmpool, chpool, dppool )

            call setmsg ( 'There is no room available for ' &
            //            'adding another numeric value ' &
            //            'to the kernel pool.' )
            call sigerr ( 'SPICE(KERNELPOOLFULL)' )
            return

         end if

!
!        Allocate a node for storing this numeric value:
!
         call lnkan ( dppool, dpnode )

         if ( datlst(nameat) == 0 ) then

!
!           There was no data for this name yet.  We make
!           DPNODE be the head of the data list for this name.
!
            datlst(nameat) =  dpnode

         else

!
!           Put this node after the tail of the current list.
!
            head =  datlst(nameat)
            tail = -dppool(prev, head )

            call lnkila ( tail, dpnode, dppool )

         end if

!
!        Finally insert this data item into the numeric buffer.
!
         dpvals(dpnode) = real( ivals(i), wp )

      end do


!
!     One last thing, see if this variable is being watched,
!     If it is, add its associated agents to the list of
!     AGENTS to be notified of a watched variable update.
!
      if (  elemc( name, wtvars )  ) then
!
!        Union the update set AGENTS with the set of agents
!        associated with the variable NAME.
!
         call zznwpool ( name,   wtvars, wtptrs, wtpool, &
                         wtagnt, active, notify, agents )

      end if


      return





!$Procedure LMPOOL ( Load variables from memory into the pool )

      entry lmpool ( cvals, n )

! $ Abstract
!
!     Load the variables contained in an internal buffer into the
!     kernel pool.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         CVALS ( * )
!     INTEGER               N
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CVALS      I   An array that contains a SPICE text kernel
!     N          I   The number of entries in CVALS.
!
! $ Detailed_Input
!
!     CVALS    is an array that contains lines of text that
!              could serve as a SPICE text kernel.
!
!     N        is the number of entries in CVALS.
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
!     1)  If any of the kernel pool variables names or their values, as
!         provided in the input CVALS array, cannot be parsed, an error
!         is signaled by a routine in the call tree of this routine.
!
!     2)  If there is no room left in the kernel pool to store all
!         variables present in the input CVALS array, an error is
!         signaled by a routine in the call tree of this routine.
!
!     3)  If the length of any kernel pool variable name present in the
!         input CVALS array exceeds its maximum allowed length (see
!         Kernel Required Reading, kernel.req), an error is signaled by
!         a routine in the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows you to store a text kernel in an internal
!     array of your program and load this array into the kernel pool
!     without first storing its contents as a text kernel.
!
! $ Examples
!
!     Suppose that your application is not particularly sensitive
!     to the current number of leapseconds but that you would
!     still like to use a relatively recent leapseconds kernel
!     without requiring users to load a leapseconds kernel into
!     the program. The example below shows how you might set up
!     the initialization portion of your program.
!
!        INTEGER               LNSIZE
!        PARAMETER           ( LNSIZE = 80 )
!
!        CHARACTER*(LNSIZE)    TEXT ( 27 )
!
!        TEXT(  1 ) = 'DELTET/DELTA_T_A =   32.184'
!        TEXT(  2 ) = 'DELTET/K         =    1.657D-3'
!        TEXT(  3 ) = 'DELTET/EB        =    1.671D-2'
!        TEXT(  4 ) = 'DELTET/M = (  6.239996D0   1.99096871D-7 )'
!        TEXT(  5 ) = 'DELTET/DELTA_AT  = ( 10,   @1972-JAN-1'
!        TEXT(  6 ) = '                     11,   @1972-JUL-1'
!        TEXT(  7 ) = '                     12,   @1973-JAN-1'
!        TEXT(  8 ) = '                     13,   @1974-JAN-1'
!        TEXT(  9 ) = '                     14,   @1975-JAN-1'
!        TEXT( 10 ) = '                     15,   @1976-JAN-1'
!        TEXT( 11 ) = '                     16,   @1977-JAN-1'
!        TEXT( 12 ) = '                     17,   @1978-JAN-1'
!        TEXT( 13 ) = '                     18,   @1979-JAN-1'
!        TEXT( 14 ) = '                     19,   @1980-JAN-1'
!        TEXT( 15 ) = '                     20,   @1981-JUL-1'
!        TEXT( 16 ) = '                     21,   @1982-JUL-1'
!        TEXT( 17 ) = '                     22,   @1983-JUL-1'
!        TEXT( 18 ) = '                     23,   @1985-JUL-1'
!        TEXT( 19 ) = '                     24,   @1988-JAN-1'
!        TEXT( 20 ) = '                     25,   @1990-JAN-1'
!        TEXT( 21 ) = '                     26,   @1991-JAN-1'
!        TEXT( 22 ) = '                     27,   @1992-JUL-1'
!        TEXT( 23 ) = '                     28,   @1993-JUL-1'
!        TEXT( 24 ) = '                     29,   @1994-JUL-1'
!        TEXT( 25 ) = '                     30,   @1996-JAN-1'
!        TEXT( 26 ) = '                     31,   @1997-JUL-1'
!        TEXT( 27 ) = '                     32,   @1999-JAN-1 )'
!
!        CALL LMPOOL ( TEXT, 27 )
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 8.3.1, 17-AUG-2021 (JDR) (BVS)
!
!        Edited the header to comply with NAIF standard.
!
!        Updated $Exceptions section to better describe the error
!        handling of this routine.
!
!-    SPICELIB Version 8.3.0, 30-JUL-2013 (BVS)
!
!        Updated to increment POOL state counter.
!
!-    SPICELIB Version 8.2.0, 10-FEB-2010 (EDW)
!
!        Added mention of the restriction on kernel pool variable
!        names to MAXLEN characters or less.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        Watcher update code was re-written for compatibility
!        with new watcher system implementation.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-&


! $ Index_Entries
!
!     Load the kernel pool from an internal text buffer
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Increment POOL state counter.
!
      call zzctrinc ( subctr )

!
!     Read from the internal SPICE pool buffer
!
      linnum = 1

      call zzrvbf ( cvals,  n,      linnum,  namlst, &
                    nmpool, pnames, datlst, &
                    dppool, dpvals, &
                    chpool, chvals, &
                    varnam, eof )


!
!     Read the variables in the file, one at a time.
!
      do while ( .not. eof .and. .not. failed() )

         if ( varnam /= ' ' ) then

            if (  elemc( varnam, wtvars )  ) then
!
!              The variable VARNAM is watched.
!
!              Union the update set AGENTS with the set of agents
!              associated with the variable VARNAM.
!
               call zznwpool ( varnam, wtvars, wtptrs, wtpool, &
                               wtagnt, active, notify, agents )

            end if

         end if
!
!        We've processed VARNAM if it was non-blank.
!
         call zzrvbf ( cvals,  n,      linnum,  namlst, &
                       nmpool, pnames, datlst, &
                       dppool, dpvals, &
                       chpool, chvals, &
                       varnam, eof )

      end do

!
!     That's it, the buffer supplied has been completely parsed
!     and placed into the kernel pool.
!
      return




!$Procedure SZPOOL (Get size limitations of the kernel pool)

      entry szpool ( name, n, found )

! $ Abstract
!
!     Return the kernel pool size limitations.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     INTEGER               N
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the parameter to be returned.
!     N          O   Value of parameter specified by NAME.
!     FOUND      O   .TRUE. if NAME is recognized.
!
! $ Detailed_Input
!
!     NAME     is the name of a kernel pool size parameter.
!              The following parameters may be specified.
!
!                 'MAXVAR'    is the maximum number of variables that
!                             the kernel pool may contain at any one
!                             time. MAXVAR should be a prime number.
!
!                 'MAXLEN'    is the maximum length of the variable
!                             names that can be stored in the kernel
!                             pool.
!
!                 'MAXVAL'    is the maximum number of distinct values
!                             that may belong to the variables in the
!                             kernel pool. Each variable must have at
!                             least one value, and may have any number,
!                             so long as the total number does not
!                             exceed MAXVAL. MAXVAL must be at least as
!                             large as MAXVAR.
!
!                 'MXNOTE'    is the maximum number of distinct
!                             variable-agents pairs that can be
!                             maintained by the kernel pool. (A variable
!                             is "paired" with an agent, if that agent
!                             is to be notified whenever the variable is
!                             updated.)
!
!                 'MAXAGT'    is the maximum number of agents that can
!                             be kept on the distribution list for
!                             notification of updates to kernel
!                             variables.
!
!                 'MAXCHR'    is the maximum number of characters that
!                             can be stored in a component of a string
!                             valued kernel variable.
!
!                 'MAXLIN'    is the maximum number of character strings
!                             that can be stored as data for kernel pool
!                             variables.
!
!              Note that the case of NAME is insignificant.
!
! $ Detailed_Output
!
!     N        is the value of the parameter specified by NAME. If
!              NAME is not one of the items specified above, N will
!              be returned with the value 0.
!
!     FOUND    is .TRUE. if the parameter is recognized .FALSE. if it
!              is not.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the specified parameter is not recognized, the value of N
!         returned will be zero and FOUND will be set to .FALSE.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides the a programmatic interface to the
!     parameters used to define the kernel pool. It is not
!     anticipated that most kernel pool users will need to use this
!     routine.
!
! $ Examples
!
!     None.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Updated NAME
!        description to not refer to the main routine.
!
!-    SPICELIB Version 1.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-&


! $ Index_Entries
!
!     return a kernel pool definition parameter
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return() ) return


      found = .true.

      if ( eqstr( name, 'MAXVAR' ) ) then
         n = maxvar
      else if ( eqstr( name, 'MAXVAL' ) ) then
         n = maxval
      else if ( eqstr( name, 'MAXLIN' ) ) then
         n = maxlin
      else if ( eqstr( name, 'MAXCHR' ) ) then
         n = maxchr
      else if ( eqstr( name, 'MXNOTE' ) ) then
         n = mxnote
      else if ( eqstr( name, 'MAXLEN' ) ) then
         n = maxlen
      else if ( eqstr( name, 'MAXAGT' ) ) then
         n = maxagt
      else
         n     = 0
         found = .false.
      end if

      return




!$Procedure DVPOOL ( Delete a variable from the kernel pool )

      entry dvpool ( name )

! $ Abstract
!
!     Delete a variable from the kernel pool.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the variable to be deleted.
!
! $ Detailed_Input
!
!     NAME     is the name of the kernel pool variable to delete.
!              The name and associated values are removed from the
!              kernel pool, freeing the occupied space.
!
!              If a watches are set on the variable designated by
!              NAME, the corresponding agents are placed on the list
!              of agents to be notified of a kernel variable update.
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
!     1)  If the specified variable is not present in the kernel pool,
!         this routine simply returns. No error is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine enables users to selectively remove variables from
!     the kernel pool, as opposed to having to clear the pool and
!     reload it.
!
!     Note that it is not necessary to remove kernel variables in order
!     to simply update them; this routine should be used only when
!     variables are to be removed.
!
! $ Examples
!
!     1)  Remove triaxial radii of Jupiter from the kernel pool.
!
!            CALL DVPOOL ( 'BODY599_RADII' )
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 8.3.1, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Moved example from $Restrictions to $Examples section.
!
!-    SPICELIB Version 8.3.0, 30-JUL-2013 (BVS)
!
!        Updated to increment POOL state counter.
!
!-    SPICELIB Version 8.2.0, 19-MAR-2009 (NJB)
!
!        Watcher update code was re-written for compatibility
!        with new watcher system implementation.
!
!-    SPICELIB Version 8.1.0, 22-DEC-2004 (NJB)
!
!        Bug fix: corrected logic for determining when a
!        conflict resolution list is non-empty.
!
!        Corrected an in-line comment relating to finding the
!        head node of the conflict resolution list for NAME.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (NJB) (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-&


! $ Index_Entries
!
!     delete a kernel pool variable
!
!-&


! $ Revisions
!
!-    SPICELIB Version 8.3.0, 30-JUL-2013 (BVS)
!
!        Updated to increment POOL state counter.
!
!-    SPICELIB Version 8.2.0, 19-MAR-2009 (NJB)
!
!        Watcher update code was re-written for compatibility
!        with new watcher system implementation.
!
!-    SPICELIB Version 8.1.0, 22-DEC-2004 (NJB)
!
!        Bug fix: corrected logic for determining when a
!        conflict resolution list is non-empty. The test
!
!           IF ( NAMEAT .LT. 0 ) THEN
!
!        formerly tested the variable NODE instead of NAMEAT.
!
!
!        Corrected an in-line comment relating to finding the
!        head node of the conflict resolution list for NAME.
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Increment POOL state counter.
!
      call zzctrinc ( subctr )

!
!     Locate the variable name in the hash table.  If the variable
!     is not present, just return.
!
!
!     Compute the hash value of this name.
!
      lookat = zzhash ( name )

!
!     Now see if there is a non-empty conflict resolution list for the
!     input string NAME.  If so, NAMLST(LOOKAT) contains the head node
!     of the conflict resolution list; this node is a positive value.
!
      if ( namlst(lookat) == 0 ) return
!
!     If were are still here NAMLST(LOOKAT) is the first node of
!     a conflict resolution list.  See if the NAME corresponding
!     to this node is the one we are looking for.
!
      nameat = namlst( lookat )
      succes = name == pnames(nameat)

      do while ( .not. succes )

         nameat = nmpool ( next, nameat )

         if ( nameat < 0 ) return

         succes = name == pnames(nameat)

      end do

!
!     Ok, the variable's here.  The head node of its value list is
!     DATLST(NAMEAT).  Delete the list pointing to the associated
!     values.  This list is in the numeric pool DPPOOL if the head
!     node is positive; otherwise the list is in the character pool
!     CHPOOL.
!
!
      call zzcln ( lookat, nameat, &
                   namlst, datlst, nmpool, chpool, dppool )

!
!     For consistency with CLPOOL, blank out the PNAMES entry containing
!     the name of this variable.  This is a bit of a flourish since
!     when errors occur during the population of the kernel pool, PNAMES
!     is not cleaned out
!
      pnames ( nameat )  =  ' '

!
!     There may be agents watching the variable we just wiped out.  If
!     so, add these agents to the list of agents to be notified of a
!     watched variable update.
!
      if (  elemc( name, wtvars )  ) then
!
!        Union the update set AGENTS with the set of agents
!        associated with the variable NAME.
!
         call zznwpool ( name,   wtvars, wtptrs, wtpool, &
                         wtagnt, active, notify, agents )

      end if

      return




!$Procedure GNPOOL (Get names of kernel pool variables)

      entry gnpool ( name, start, room, n, cvals, found )

! $ Abstract
!
!     Return names of kernel variables matching a specified template.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     INTEGER               START
!     INTEGER               ROOM
!     INTEGER               N
!     CHARACTER*(*)         CVALS    ( * )
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Template that names should match.
!     START      I   Index of first matching name to retrieve.
!     ROOM       I   The largest number of values to return.
!     N          O   Number of values returned for NAME.
!     CVALS      O   Kernel pool variables whose names match NAME.
!     FOUND      O   .TRUE. if there is at least one match.
!
! $ Detailed_Input
!
!     NAME     is a MATCHI template which will be used when searching
!              for variable names in the kernel pool. The characters
!              '*' and '%' are used for the wild string and wild
!              characters respectively. For details of string
!              pattern matching see the header of the routine MATCHI.
!
!
!     START    is the index of the first variable name to return that
!              matches the NAME template. The matching names are
!              assigned indices ranging from 1 to NVAR, where NVAR is
!              the number of matching names. The index of a name does
!              not indicate how it compares alphabetically to another
!              name.
!
!              If START is less than 1, it will be treated as 1. If
!              START is greater than the total number of matching
!              variable names, no values will be returned and N will
!              be set to zero. However, FOUND will still be set to
!              .TRUE.
!
!
!     ROOM     is the maximum number of variable names that should
!              be returned for this template. If ROOM is less than 1
!              the error SPICE(BADARRAYSIZE) will be signaled.
!
! $ Detailed_Output
!
!     N        is the number of variable names matching NAME that are
!              returned. It will always be less than or equal to
!              ROOM.
!
!              If no variable names match NAME, N is set to zero.
!
!
!     CVALS    is an array of kernel pool variables whose names match
!              the template NAME and which have indices ranging from
!              START to START+N-1.
!
!              Note that in general the names returned in CVALS are
!              not sorted.
!
!              If no variables match NAME, no values are assigned to
!              the elements of CVALS.
!
!              If the length of CVALS is less than the length of the
!              variable names, the values returned will be truncated
!              on the right. To ensure that names are not truncated,
!              CVALS should be declared to be at least
!              CHARACTER*(32).
!
!
!     FOUND    is .TRUE. if the some variable name in the kernel pool
!              matches NAME, .FALSE. if it is not.
!
! $ Parameters
!
!     MAXLEN   is the maximum length of the variable names that
!              can be stored in the kernel pool. This value is
!              currently 32.
!
! $ Exceptions
!
!     1)  If the value of ROOM is less than one, the error
!         SPICE(BADARRAYSIZE) is signaled.
!
!     2)  If CVALS has declared length less than the size of a variable
!         name to be returned, the name will be truncated on the right.
!         See MAXLEN for the maximum size of variable names.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides the user interface for retrieving the names
!     of kernel pool variables. This interface allows you to retrieve
!     the names matching a template via multiple accesses. Under some
!     circumstances this alleviates the problem of having to know in
!     advance the maximum amount of space needed to accommodate all
!     matching names.
!
!     However, this method of access does come with a price. It is
!     always more efficient to retrieve all of the data associated with
!     a kernel pool variable in one call than it is to retrieve it in
!     sections. The parameter MAXVAR defines the upper bound on the
!     number of possible matching names.
!
! $ Examples
!
!     The following code fragment demonstrates how the names of kernel
!     pool variables matching a template can be retrieved in pieces.
!
!     First we need some declarations.
!
!        INTEGER               ROOM
!        PARAMETER           ( ROOM = 3 )
!
!        CHARACTER*(3)         INDENT
!        CHARACTER*(80)        CVALS  (ROOM)
!        CHARACTER*(8)         VARNAM
!
!        INTEGER               START
!        INTEGER               N
!
!        LOGICAL               FOUND
!
!
!     Next load the data in the file 'typical.ker' into the
!     kernel pool.
!
!        CALL LDPOOL ( 'typical.ker' )
!
!     Next we shall print the names of kernel variables that match the
!     template 'BODY599*'.
!
!        VARNAM = 'BODY599*'
!        INDENT = ' '
!        START  =  1
!
!        CALL GNPOOL ( VARNAM, START, ROOM, N, CVALS, FOUND )
!
!        IF ( .NOT. FOUND ) THEN
!
!           WRITE (*,*) 'There are no matching variables ' //
!       .               'in the kernel pool.'
!        ELSE
!
!           WRITE (*,*) 'Kernel pool variables:'
!           WRITE (*,*)
!
!           DO I = 1, N
!              WRITE (*,*) INDENT, CVALS(I)
!           END DO
!
!           DO WHILE ( N .EQ. ROOM )
!
!              START = START + N
!              CALL GNPOOL ( VARNAM, START, ROOM, N, CVALS, FOUND )
!
!              DO I = 1, N
!                 WRITE (*,*) INDENT, CVALS(I)
!              END DO
!
!           END DO
!
!        END IF
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
!
! $ Version
!
!-    SPICELIB Version 8.1.1, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added MAXLEN
!        description in $Parameters.
!
!-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
!
!        ZZPINI call was updated for compatibility
!        with new watcher system implementation.
!
!-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!
!        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!        direct insertion of data into the kernel pool without having
!        to read an external file.
!
!        Added the interface LMPOOL that allows SPICE
!        programs to load text kernels directly from memory
!        instead of requiring a text file.
!
!        Added the entry point SZPOOL to return kernel pool definition
!        parameters.
!
!        Added the entry point DVPOOL to allow the removal of a variable
!        from the kernel pool.
!
!        Added the entry point GNPOOL to allow users to determine
!        variables that are present in the kernel pool
!
!-&


! $ Index_Entries
!
!     return names of kernel pool variables matching a template
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return() ) return



!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Perform the one obvious error check first.
!
      if ( room < 1 ) then

         call setmsg ( 'The amount of room specified as ' &
         //            'available for output in the output array ' &
         //            'was: #.  The amount of room must be ' &
         //            'positive. ' )


         call errint ( '#', room )
         call sigerr ( 'SPICE(BADARRAYSIZE)'  )
         return

      end if

!
!     So far we've encountered no matching names.
!
      hits  = 0
      n     = 0
      begin = max( 1, start )

      do k = 1, maxvar
!
!        See if there is any variable associated with this hash value.
!
         nnode = namlst(k)

         do while (  nnode > 0 )
!
!           There is some name list associated with this node. See if
!           it the current one matches the supplied template.
!
            if ( matchi( pnames(nnode), name, '*', '%' ) ) then

!
!              We've got a match.  Record this fact and if we have
!              reached (or passed) the starting point, put this name
!              on the output list.
!
               hits = hits + 1

               if ( hits >= start ) then

                  if ( n < room ) then
                     n        = n + 1
                     cvals(n) = pnames(nnode)
                  end if
!
!                 If we've filled up the buffer, we may as well
!                 quit now.
!
                  if ( n == room ) then
                     found = .true.
                     return
                  end if

               end if

            end if

!
!           Get the next name for this node.
!
            nnode = nmpool(next,nnode)

         end do
!
!        Advance to the next hash value.
!
      end do


      found = hits > 0

      return



!$Procedure DWPOOL ( Delete watch from kernel pool )

      entry dwpool ( agent )

! $ Abstract
!
!     Delete a name from the list of agents to notify whenever a member
!     of a list of kernel variables is updated.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     CONSTANTS
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         AGENT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     AGENT      I   The name of an agent to be notified after updates.
!
! $ Detailed_Input
!
!     AGENT    is any agent name that has previously been associated
!              with a kernel pool watch via a call to SWPOOL. The
!              agent name will be deleted from the notification list
!              of every watched kernel variable.
!
!              Watched variables whose notification lists become
!              empty will be deleted.
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
!     1)  It's not an error to delete a non-existent agent---one
!         that is not present in the watcher system. A call to
!         delete a non-existent agent has no effect on the state
!         of the watcher system.
!
!     2)  If an attempt is made to delete an agent that
!         has an unchecked update, the error SPICE(UPDATEPENDING)
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Kernel pool watches are a limited resource; the ability
!     to delete watches when they're no longer needed is essential
!     to allow programs that make heavy use of kernel pool watches
!     to run for extended periods.
!
! $ Examples
!
!     Suppose that you have an application subroutine, MYTASK, that
!     needs to access a large data set in the kernel pool. If this
!     data could be kept in local storage and kernel pool queries
!     performed only when the data in the kernel pool has been
!     updated, the routine can perform much more efficiently.
!
!     If at some point the local stored data no longer need to be
!     watched---for example, if they're removed from the local
!     buffer to make room for other data---the watch set by the
!     agent 'MYTASK' on those data can be deleted via the call
!
!        CALL DWPOOL ( 'MYTASK' )
!
! $ Restrictions
!
!     1)  It is recommended that watches be deleted only by
!         routines that established them.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 11-SEP-2013 (BVS)
!
!        Updated to increment POOL state counter. Updated description
!        of the exception 1).
!
!-    SPICELIB Version 1.0.0, 19-MAR-2009 (NJB)
!
!-&


! $ Index_Entries
!
!     delete kernel pool watch
!     delete agent from kernel pool watch lists
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return() ) return


!
!     Initialize the pool if necessary.
!
      if ( first ) then

         call zzpini (  first, &
                        maxvar, maxval, maxlin, &
                        begdat, begtxt, &
                        nmpool, dppool, chpool, &
                        namlst, datlst, &
                        maxagt, mxnote, wtvars, &
                        wtptrs, wtpool, wtagnt, &
                        agents, active, notify, subctr )

      end if

!
!     Make sure we're not silencing an agent who has something
!     to say.
!
      if (  elemc( agent, agents )  ) then

         call setmsg ( 'Could not delete AGENT # from the watch ' &
         //            'symbol table because AGENT is associated ' &
         //            'with at least one updated kernel variable. ' )
         call errch  ( '#', agent                                    )
         call sigerr ( 'SPICE(UPDATEPENDING)'                        )
         return

      end if

!
!     Increment POOL state counter.
!
      call zzctrinc ( subctr )

!
!     AGENT is no longer on the list of agents associated with a
!     kernel variable update.
!
      call removc ( agent, agents )

!
!     For each kernel variable in the watcher's list, remove
!     AGENT from its list of guys to be notified when a variable change
!     occurs. If AGENT is the only value associated with the variable,
!     delete the kernel variable's entry from the table.
!
!     This outer loop is relatively tricky, since
!
!        1) The upper loop bound can change during loop execution.
!
!        2) The loop index I doesn't necessary increase on every
!           loop pass.
!
!     Infinite loops can lurk in code with the above attributes. We
!     need to know that the loop will always terminate. Presume that
!     no SPICE error occurs during the loop: then we observe
!     that on each loop pass, either I increases or the loop bound
!     CARDC(WTVARS) decreases, so the difference
!
!        CARDC(WTVARS) - I
!
!     does in fact decrease on every loop iteration. When this
!     difference becomes -1, the loop will end.
!
!     If a SPICE error occurs during the loop, the FAILED test
!     will terminate the loop.
!
!     Since WTVARS may shrink due to deletion of watches, we
!     fetch the cardinality of WTVARS on each loop iteration.
!
      i = 1

      do while (  ( i <= cardc(wtvars) )  .and.  ( .not. failed() )  )
!
!        Search the list of agents associated with the Ith watched
!        variable for AGENT. We want the list count as well, so
!        we'll traverse the whole list (which likely is short).
!
!        We don't use ZZGAPOOL here because we need to get the
!        watcher pool nodes associated with AGENT.
!
!        If we find AGENT, we'll use AGNODE to designate
!        the node associated with AGENT.
!
         node   = wtptrs(i)
         nnodes = 0
         agnode = 0

         do while ( node > 0 )

            nnodes = nnodes + 1
!
!           Fetch the next agent for the Ith kernel variable.
!
            if ( wtagnt(node) == agent ) then
!
!               Save the current node.
!
                agnode = node

            end if
!
!           Find the next node in the list.
!
            node = lnknxt( node, wtpool )

         end do

         if ( agnode > 0 ) then
!
!           The input agent is on the agent list for the Ith watched
!           kernel variable. Delete this agent from the list. Delete
!           the node corresponding to AGENT from the watch pool. First
!           set the corresponding agent name to blank.
!
            wtagnt(agnode) = ' '

!
!           If we're about to delete the head node of the agent list,
!           we'll need to update WTPTRS(I) to point to the new head.
!           It's possible that this agent list is empty after deletion
!           of AGNODE; we'll handle that case after the LNKFSL call
!           below.
!
            if ( wtptrs(i) == agnode ) then

               wtptrs(i) = lnknxt( agnode, wtpool )

            end if

!
!           Now free AGNODE.
!
            call lnkfsl ( agnode, agnode, wtpool )


            if ( nnodes == 1 ) then
!
!              In fact AGENT is the *only* agent for the Ith variable.
!              Deleting AGENT means that nobody's watching this
!              variable any more, so delete the variable from the
!              watched variable set.

               nw     = cardc( wtvars )

               varnam = wtvars(i)

               call removc ( varnam, wtvars )

!
!              Remove the associated pointer from the pointer array.
!
               call remlai ( 1, i, wtptrs, nw )

!
!              Since we deleted the current variable table entry and
!              compressed the set WTVARS and the array WTPTRS, I now
!              points to the next variable in the table. Decrement I
!              here to compensate for the increment operation at the
!              bottom of the loop.
!
               i = i - 1

            end if
!
!           We've now deleted AGENT from the AGENT list for WTVARS(I).
!           If the deletion left no agents watching WTVARS(I), we
!           deleted WTVARS(I) and its associated pointer WTPTRS(I).
!
         end if
!
!        We've processed the Ith kernel variable in the watcher table.
!
!        If we deleted the Ith WTVARS entry, we decremented I
!        at that time, so the increment operation here always is
!        applicable.
!
         i = i + 1

!
!        At this point in the loop, either I has increased or
!        CARDC(WTVARS) has decreased; hence we've made progress
!        toward loop termination.
!
      end do

      return




!$Procedure ZZVUPOOL ( Private: view kernel pool watch system )

      entry zzvupool ( uwvars, uwptrs, uwpool, uwagnt )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return copies of certain POOL variables that are used in the
!     implementation of the watcher mechanism. These variables
!     comprise a data structure that maps kernel variable names to sets
!     of agents watching those kernel variables.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     KERNEL
!     PRIVATE
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         UWVARS    ( LBCELL : * )
!     INTEGER               UWPTRS    ( * )
!     INTEGER               UWPOOL    ( 2, LBCELL : * )
!     CHARACTER*(*)         UWAGNT    ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UWVARS     O   Watched kernel variable set.
!     UWPTRS     O   Pointers from variables into the watch pool.
!     UWPOOL     O   Watch pool used for managing agent names.
!     UWAGNT     O   Array of agent names.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     UWVARS   is a set into which the local watcher system
!              set WTVARS has been copied.
!
!     UWPTRS   is an array into which the local watcher system
!              array WTPTRS has been copied.
!
!     UWPOOL   is a doubly linked list pool into which the local
!              watcher system doubly linked list pool WTPOOL has
!              been copied.
!
!     UWAGNT   is an array into which the local watcher system
!              array WTAGNT has been copied.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the output array UWVARS is too small to hold the
!         set WTVARS, an error is signaled by a routine
!         in the call tree of this routine.
!
!     2)  If any output array other than UWVARS is to small
!         to hold the corresponding watch system component,
!         memory corruption will occur.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is not part of the SPICELIB API. This routine
!     may be removed in a later version of the SPICE Toolkit, or
!     its interface may change.
!
!     SPICE-based application code should not call this routine.
!
!     This is an "inspection hatch" routine used for SPICELIB
!     testing.
!
! $ Examples
!
!     See the TSPICE test family F_DWPOOL.
!
! $ Restrictions
!
!     1)  This is a private routine. See $Particulars above.
!
!     2)  The caller must provide output arrays of adequate
!         size. See the declarations of the watch system
!         components in the umbrella routine POOL for size
!         requirements.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 27-MAR-2014 (BVS)
!
!        Set $Index_Entries to "None." to make this entry no appear in
!        permuted index.
!
!-    SPICELIB Version 1.0.0, 19-MAR-2009 (NJB)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return() ) return


      call copyc ( wtvars, uwvars                )
      call movei ( wtptrs, cardc(wtvars), uwptrs )

!
!     UWPOOL is expected to have dimensions
!
!        ( 2,  LBPOOL : MXNOTE )
!

      i = 2 * ( 6 + mxnote )

      call movei ( wtpool, i, uwpool )

      call movec ( wtagnt, mxnote, uwagnt )


      return


!$Procedure ZZPCTRCK ( Private: check/update user's POOL state counter )

      entry zzpctrck ( usrctr, update )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Check and update the POOL state counter tracked by a caller
!     (user) routine.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     KERNEL
!     PRIVATE
!     UTILITY
!
! $ Declarations
!
!     INTEGER               USRCTR    ( CTRSIZ )
!     LOGICAL               UPDATE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     USRCTR    I-O  POOL state counter tracked by the caller
!     UPDATE     O   Flag indicating if input counter was updated
!
!     CTRSIZ     P   Dimension of the counter array
!
! $ Detailed_Input
!
!     USRCTR   is the value of the POOL state counter tracked by
!              (saved in) the caller (user) routine.
!
! $ Detailed_Output
!
!     USRCTR   is the current POOL state counter.
!
!     UPDATE   is the logical flag indicating whether the input POOL
!              state counter was different from the current POOL
!              state counter and, therefore, had to be updated
!              (UPDATE = .TRUE.) or if it was the same (UPDATE =
!              .FALSE.).
!
! $ Parameters
!
!     CTRSIZ   is the dimension of the counter array used by
!              various SPICE subsystems to uniquely identify
!              changes in their states. This parameter is
!              defined in the private include file 'zzctr.inc'.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is not part of the SPICELIB API. This routine
!     may be removed in a later version of the SPICE Toolkit, or
!     its interface may change.
!
!     SPICE-based application code should not call this routine.
!
!     This routine allows other routines to be aware of POOL state
!     change due to addition or deletion variables or watchers. Such
!     awareness is needed to be able to locally save some POOL-based
!     data (e.g. body name/ID mappings or frame definitions) and only
!     update these locally saved values if the POOL has changed.
!
!     To make use of the POOL state counter to achieve this goal the
!     caller routines save the POOL state counter returned by the first
!     call to this routine and then check that saved value against the
!     current POOL state counter and update it by subsequent calls this
!     routine.
!
! $ Examples
!
!     The routines that need to be aware of and act on the POOL state
!     change initialize a local POOL counter array using ZZCTRUIN, save
!     it, and check it against the current POOL state counter and
!     update it, if needed, using this entry point, as follows:
!
!        C
!        C     Include zzctr.inc to access CTRSIZ.
!        C
!              INCLUDE              'zzctr.inc'
!              ...
!
!        C
!        C     In local variable declarations declare and save
!        C     the local POOL state counter. Also declare the
!        C     update flag.
!        C
!              INTEGER               USRCTR ( CTRSIZ )
!              LOGICAL               UPDATE
!              ...
!              SAVE                  USRCTR
!              ...
!
!        C
!        C     In all places where initialization is done
!        C     initialize the local POOL state counter using
!        C     ZZCTRUIN to ensure an update on the first check.
!        C
!              IF ( FIRST ) THEN
!                 ...
!                 CALL ZZCTRUIN( USRCTR )
!                 FIRST = .FALSE.
!              END IF
!              ...
!
!        C
!        C     In all places where there is a need to check for
!        C     the POOL state change call this entry to
!        C     check and update the local POOL state counter.
!        C
!              CALL ZZPCTRCK ( USRCTR, UPDATE )
!
!              IF ( UPDATE ) THEN
!
!        C
!        C        It the POOL state changed, do what needs to
!        C        be done to deal with saved values based
!        C        on POOL data.
!        C
!                 ...
!
!              END IF
!
! $ Restrictions
!
!     1)  This is a private routine. See $Particulars above.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 27-MAR-2014 (BVS)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

      call zzctrchk ( subctr, usrctr, update )

      return
      end
