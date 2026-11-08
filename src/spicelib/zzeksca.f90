!$Procedure      ZZEKSCA ( EK, scratch area )

      subroutine zzeksca ( n, beg, end, idata, top )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Manage the EK scratch area.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     UTILITY
!
! $ Declarations

      integer(ip)           n
      integer(ip)           beg
      integer(ip)           end
      integer(ip)           idata ( * )
      integer(ip)           top

! $ Brief_I/O
!
!     Variable  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     N          I   ZZEKSPSH, ZZEKSPOP, ZZEKSDEC
!     BEG        I   ZZEKSUPD, ZZEKSRD
!     END        I   ZZEKSUPD, ZZEKSRD
!     IDATA     I-O  ZZEKSPSH, ZZEKSPOP, ZZEKSUPD, ZZEKSRD
!     TOP        O   ZZEKSTOP
!
! $ Detailed_Input
!
!     See the entry points for descriptions of their inputs.
!
! $ Detailed_Output
!
!     See the entry points for descriptions of their outputs.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If this routine is called directly, the error SPICE(BOGUSENTRY)
!        is signaled.
!
!     See the entry points for discussions of exceptions specific to
!     those routines.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The specific implementation of the EK scratch area is NOT
!     considered part of the specification of this suite of routines:
!     the implementation may be changed without notice.  However,
!     some aspects of the current implementation, such as scratch
!     file usage, are visible to users and therefore are discussed
!     in this subroutine header.
!
!     The EK system, in searching for events that satisfy a query,
!     produces intermediate results that require a potentially very
!     large amount of storage, more than can be expected to be
!     available in the form of memory.  On the other hand, in order
!     to achieve reasonable query response time, these intermediate
!     results must be capable of being accessed quickly.  The EK
!     scratch area provides a storage location that uses a combination
!     of memory and disk storage to give the EK system a large storage
!     area, part of which can be rapidly accessed.
!
!     The logical structure of the EK scratch area is that of a large
!     one-dimensional integer stack.  The indices of the  elements of
!     this stack are referred to as scratch area `addresses'.  Scratch
!     area addresses start at 1 and increase.  The maximum address is
!     the maximum integer representable on the host computer, but the
!     maximum usable address depends on the disk storage available
!     to the calling program at the time the program is run.
!
!     The EK scratch area has access routines that allow a calling
!     program to write to and read from it. Calling routines must
!     coordinate their use of the scratch area:  the scratch area is
!     effectively a global data structure.  Routines outside of the EK
!     system should not use the scratch area.
!
!     The EK scratch area routines are:
!
!        ZZEKSCA  ( EK scratch area umbrella routine )
!        ZZEKSTOP ( EK scratch area, return stack pointer )
!        ZZEKSPSH ( EK scratch area, push data onto stack )
!        ZZEKSDEC ( EK scratch area, decrement stack pointer )
!        ZZEKSPOP ( EK scratch area, pop data from stack )
!        ZZEKSUPD ( EK scratch area, update data )
!        ZZEKSRD  ( EK scratch area, read data )
!        ZZEKSCLN ( EK scratch area, clean up )
!
! $ Examples
!
!     1)  Push data on the scratch area stack.
!
!            C
!            C     Push N items onto the stack.
!            C
!                  CALL ZZEKSPSH ( N, DATA )
!
!
!     2)  Update a range of addresses that may span the stack top.
!
!            C
!            C     Since we can't leave a gap between the stack top
!            C     and the start of the range of addresses we write to,
!            C     we'll need to know where the top is.  The address
!            C     range to update is BEG:END.
!            C
!                  CALL ZZEKSTOP ( TOP )
!
!                  IF ( BEG .GT. TOP ) THEN
!
!                     [ Handle error case ]
!
!                  ELSE
!
!                     CALL ZZEKSUPD ( BEG, END, DATA )
!
!                  END IF
!
!
!
!     3)  Read from the scratch area.
!
!            C
!            C     Read the contents of the scratch area address
!            C     range BEG:END into the integer array DATA:
!            C
!                  CALL ZZEKSTOP ( TOP )
!
!                  IF ( BEG .GT. TOP ) THEN
!
!                     [ Handle error case ]
!
!                  ELSE
!
!                     CALL ZZEKSRD ( BEG, END, DATA )
!
!                  END IF
!
! $ Restrictions
!
!     1)  The current implementation of this suite of routines opens
!         a scratch file.  The logical unit connected to the scratch
!         file counts against the total that may be used by the calling
!         program.  Also, the scratch file, if written to, will occupy
!         additional disk storage.
!
!     2)  This suite of routines should not be used by routines outside
!         of the EK system.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.2.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    SPICELIB Version 3.2.0, 28-JUN-2005 (NJB)
!
!        Increased buffer size from 500K to 2M integers.
!
!-    SPICELIB Version 3.1.0, 29-JUL-2003 (NJB)
!
!        Added DASWBR call to entry point ZZEKCLN.  This call frees
!        the buffer records used by the scratch file.
!
!-    SPICELIB Version 3.0.0, 13-DEC-2001 (NJB)
!
!        Added entry point ZZEKCLN.
!
!-    Beta Version 2.0.0, 02-NOV-1995 (NJB)
!
!        Updated for EK architecture 3.
!
!-    Beta Version 1.1.0, 01-AUG-1994 (NJB)
!
!        Scratch area buffer size increased to 500K integers.
!        On 32-bit systems, this amounts to 2Mb of storage.
!
!-    Beta Version 1.0.1, 25-FEB-1993 (NJB)
!
!        Documented.
!
!-    Beta Version 1.0.0, 16-DEC-1992 (NJB)
!
!-&

! $ Index_Entries
!
!     manage the EK scratch area
!
!-&

! $ Revisions
!
!-    SPICELIB Version 3.2.0, 28-JUN-2005 (NJB)
!
!        Increased buffer size from 500K to 2M integers.
!
!-    Beta Version 2.0.0, 08-SEP-1994 (NJB)
!
!        Updated for EK architecture 3.
!
!-    Beta Version 1.1.0, 01-AUG-1994 (NJB)
!
!        Scratch area buffer size increased to 500K integers.
!        On 32-bit systems, this amounts to 2Mb of storage.
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

!
!     The parameter MEMSIZ is the size of an integer array used as
!     part of the scratch area.  The first MEMSIZ scratch area addresses
!     refer to elements of this array.  Additional storage is supplied
!     by the integer logical array of a scratch DAS file; the first
!     word of the scratch DAS file corresponds to scratch area address
!     MEMSIZ + 1.
!
      integer(ip)           memsiz
      parameter           ( memsiz = 2500000 )



!
!     Local variables
!
      integer(ip)           b
      integer(ip)           base
      integer(ip)           e
      integer(ip)           i
      integer(ip)           lastc
      integer(ip)           lastd
      integer(ip)           lasti
      integer(ip)           numadd
      integer(ip)           numrd
      integer(ip)           rb
      integer(ip)           remain
      integer(ip)           rt
      integer(ip)           scrhan
      integer(ip)           scrtch ( memsiz )
      integer(ip)           start
      integer(ip)           t

      logical               first


!
!     Saved variables
!
      save

!
!     Initial values
!
      data                  first  / .true.  /
      data                  t      /  0      /



!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     This routine should never be called directly.
!
      call sigerr ( 'SPICE(BOGUSENTRY)' )

      return




!$Procedure    ZZEKSTOP  ( EK scratch area, stack top )

      entry zzekstop ( top )

! $ Abstract
!
!     Obtain last address in use in EK scratch area.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     UTILITY
!
! $ Declarations
!
!     INTEGER               TOP
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TOP        O   EK scratch area stack top.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     TOP            is the last address of the EK scratch area stack
!                    top.  This is the highest EK scratch area address
!                    currently in use.
!
! $ Parameters
!
!     None.
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
!     The EK scratch area stack top ranges from zero to, theoretically,
!     the largest integer representable on the host system.
!     and never decreases during a program run.  Data pushed on the
!     EK stack is inserted at address TOP+1 and occupies a contiguous
!     range of addresses that extends upwards from this address.
!
! $ Examples
!
!     See the header of the umbrella routine ZZEKSCA for an example
!     of use of this routine.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    Beta Version 2.0.0, 08-SEP-1994 (NJB)
!
!        Updated for EK architecture 3.
!
!-    Beta Version 1.0.0, 25-FEB-1993 (NJB)
!
!-&

! $ Index_Entries
!
!     read from EK scratch area
!
!-&

      top  =  t
      return






!$Procedure    ZZEKSPSH  ( EK scratch area, push data )

      entry zzekspsh ( n, idata )

! $ Abstract
!
!     Push the contents of an integer array onto the EK scratch area
!     stack.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     UTILITY
!
! $ Declarations
!
!     INTEGER               N
!     INTEGER               IDATA ( * )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     N          I   Number of integers to push.
!     IDATA      I   Integer data.
!
! $ Detailed_Input
!
!     N              is the number of integers in the array IDATA to
!                    append to the EK scratch area.  The data is
!                    stored in scratch area addresses T+1:T+N,
!                    where T is the EK scratch area stack top prior to
!                    the call to ZZEKSPSH.
!
!     IDATA          is an integer array containing data to append to
!                    the EK scratch area.  The first N elements of
!                    IDATA are appended to the EK scratch area, in
!                    order.
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
!     1)  If N is non-positive, this routine simply returns.  No error
!         is signaled.
!
!     2)  If an I/O error occurs during the data addition, the error
!         will be diagnosed by routines called by this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Let TOP be the EK scratch area stack top prior to a call to this
!     routine.  Data that is appended to the EK scratch area by this
!     routine is inserted at address TOP+1 and occupies a contiguous
!     range of addresses that extends upwards from this address.
!
!     As a side effect of calling this routine, TOP is set to TOP + N.
!
! $ Examples
!
!     See the header of the umbrella routine ZZEKSCA for an example
!     of use of this routine.
!
! $ Restrictions
!
!     1)  This routine must execute quickly.  Therefore, it checks in
!         only if it detects an error.  If an error is signaled by a
!         routine called by this routine, this routine will not appear
!         in the SPICELIB traceback display.  Also, in the interest
!         of speed, this routine does not test the value of the SPICELIB
!         function RETURN upon entry.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 15-JAN-1995 (NJB)
!
!-&



! $ Index_Entries
!
!     push integer data onto EK scratch area stack
!
!-&

!
!     No checking in here.
!

!
!     First time through, open a scratch DAS file.
!
      if ( first ) then

         first = .false.

         call dasops ( scrhan )

         if ( failed() ) return

      end if

!
!     Go back if there's no data to write.
!
      if ( n < 1 ) return

!
!     Add as much data as possible to our big array.
!
      if ( t < memsiz ) then

         numadd = min ( n, memsiz-t )

         do i = 1, numadd
            scrtch(t+i) = idata(i)
         end do

         t      = t + numadd

         if ( numadd == n ) return

         remain = n - numadd
         start  = 1 + numadd

         if ( remain == 0 ) return

      else

         remain = n
         start  = 1

      end if
!
!     At this point, REMAIN and START are set, and T reflects the
!     amount of data we've pushed so far.  If we got this far,
!     we'll need to put the rest of the data in the scratch DAS.
!
!     The DAS system requires separate operations for updating
!     an existing range of addresses and for appending data.
!     We need to know the last integer address in use in the DAS
!     file in order to determine which part of the data will
!     be written to addresses previously written to, and which
!     part will be appended.
!
      call daslla ( scrhan, lastc, lastd, lasti )

!
!     To simplify our arithmetic, we'll work with a variable RT
!     that represents the stack top measured relative to the base
!     of the DAS integer array.  At this point, RT is greater than
!     or equal to zero.
!
      rt  =  t - memsiz

      if ( rt < lasti ) then
!
!        Some data can be added by updating DAS addresses.  The
!        available range for updating is B:E, where B and E are
!        calculated below.  This case can occur only when LASTI > 0.
!
         b       =  rt + 1
         e       =  min ( lasti,  rt + remain )

         call dasudi ( scrhan, b, e, idata(start) )

         numadd  =  e      - b + 1
         start   =  start  + numadd
         remain  =  remain - numadd
         t       =  t      + numadd

         if ( remain == 0 ) return

      end if

!
!     At this point, START and REMAIN are set, and T reflects the
!     amount of data we've pushed so far..  The remaining data
!     must be appended to the scratch DAS file.
!
      call dasadi ( scrhan, remain, idata(start) )

      t = t + remain

      return




!$Procedure    ZZEKSPOP  ( EK scratch area, pop data )

      entry zzekspop ( n, idata )

! $ Abstract
!
!     Pop a specified number of elements from the top of the EK scratch
!     area stack, transferring this data to an integer array.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     UTILITY
!
! $ Declarations
!
!     INTEGER               N
!     INTEGER               IDATA ( * )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     N          I   Number of integers to pop.
!     IDATA      O   Integer data.
!
! $ Detailed_Input
!
!     N              is the number of integers to pop from the
!                    EK scratch area stack.  The data is
!                    read from the scratch area addresses T-N+1:T,
!                    where T is the stack top prior to the call to
!                    ZZEKSPOP.
!
! $ Detailed_Output
!
!     IDATA          is an integer array containing data read from
!                    the EK scratch area.  The first N elements of
!                    IDATA assigned the values occupying the top N
!                    elements of the EK stack.
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If N is non-positive or if N is greater than the number of
!         items on the stack, the error SPICE(INVALIDCOUNT) is
!         signaled.
!
!     2)  If an I/O error occurs during the data read, the error
!         will be diagnosed by routines called by this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Let TOP be the EK scratch area stack top prior to a call to this
!     routine.  Data that is read from the EK scratch area by this
!     routine is transferred from addresses TOP-N+1 to TOP and occupies
!     to the range of addresses 1 to N in the array IDATA.
!
!     As a side effect of calling this routine, TOP is set to TOP - N.
!
! $ Examples
!
!     See the header of the umbrella routine ZZEKSCA for an example
!     of use of this routine.
!
! $ Restrictions
!
!     1)  This routine must execute quickly.  Therefore, it checks in
!         only if it detects an error.  If an error is signaled by a
!         routine called by this routine, this routine will not appear
!         in the SPICELIB traceback display.  Also, in the interest
!         of speed, this routine does not test the value of the SPICELIB
!         function RETURN upon entry.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 10-SEP-1994 (NJB)
!
!-&



! $ Index_Entries
!
!     pop integer data from EK scratch area stack
!
!-&

!
!     No checking in here.
!

!
!     First time through, open a scratch DAS file.
!
      if ( first ) then

         first = .false.

         call dasops ( scrhan )

         if ( failed() ) return

      end if

!
!     You can't pop a negative number of elements.
!
      if ( n < 0 ) then

         call setmsg ( 'Pop count must be non-negative; '    // &
                       'call requests popping # elements.'    )
         call errint ( '#', n                                 )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                  )
         return

!
!     It's an error to try to pop more data than we have on the
!     stack.
!
      else if ( n > t ) then

         call setmsg ( 'EK stack pointer = #; call requests popping ' // &
                       '# items.'                                      )
         call errint ( '#', t                                          )
         call errint ( '#', n                                          )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                           )
         return

      end if


!
!     Read as much data as possible from our big array.
!
      base  = t - n

      if ( base < memsiz ) then

         numrd = min ( n, memsiz-base )

         do i = 1, numrd
            idata(i) = scrtch(base+i)
         end do

         if ( numrd == n ) then

            t  =  t - numrd
            return

         end if

         remain = n      -  numrd
         base   = memsiz
         start  = numrd  +  1

      else

         remain = n
         start  = 1

      end if

!
!     At this point, REMAIN, START and BASE are set.  If we got this
!     far, we'll need to read the rest of the data from the scratch DAS.
!     Compute the base address to read from relative to the start of
!     the DAS array.
!
      rb     =  base - memsiz
      b      =  rb   + 1
      e      =  rb   + remain

      call dasrdi ( scrhan, b, e, idata(start) )

      t      =  t - n

      return






!$Procedure    ZZEKSDEC  ( EK scratch area, decrement stack pointer )

      entry zzeksdec ( n )

! $ Abstract
!
!     Decrement the EK scratch area stack pointer by a specified count.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     UTILITY
!
! $ Declarations
!
!     INTEGER               N
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     N          I   Decrement count.
!
! $ Detailed_Input
!
!     N              is the number to subtract from the EK scratch
!                    area stack pointer.
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
!     1)  If N is non-positive or if N is greater than the number of
!         items on the stack, the error SPICE(INVALIDCOUNT) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Let TOP be the EK scratch area stack top prior to a call to this
!     routine.  The  effect of calling this routine is that TOP is set
!     to TOP - N.
!
! $ Examples
!
!     See the header of the umbrella routine ZZEKSCA for an example
!     of use of this routine.
!
! $ Restrictions
!
!     1)  This routine must execute quickly.  Therefore, it checks in
!         only if it detects an error.  If an error is signaled by a
!         routine called by this routine, this routine will not appear
!         in the SPICELIB traceback display.  Also, in the interest
!         of speed, this routine does not test the value of the SPICELIB
!         function RETURN upon entry.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 10-SEP-1994 (NJB)
!
!-&



! $ Index_Entries
!
!     decrement EK scratch area stack pointer
!
!-&

!
!     No checking in here.
!

!
!     First time through, open a scratch DAS file.
!
      if ( first ) then

         first = .false.

         call dasops ( scrhan )

         if ( failed() ) return

      end if


!
!     Catch non-positive decrement requests.
!
      if ( n < 0 ) then

         call setmsg ( 'Decrement value must be non-negative; ' // &
                       'call requests decrement by #.'          )
         call errint ( '#', n                                   )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                    )
         return

!
!     It's an error to try to decrement the pointer by more than
!     the current stack depth.
!
      else if ( n > t ) then

         call setmsg ( 'EK stack pointer = #; call requests  ' // &
                       'decrement by #.'                        )
         call errint ( '#', t                                   )
         call errint ( '#', n                                   )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                    )
         return

      end if


      t  =  t  -  n

      return




!$Procedure    ZZEKSUPD  ( EK scratch area, update )

      entry zzeksupd ( beg, end, idata )

! $ Abstract
!
!     Update the contents of a range of addresses already in use in the
!     EK scratch area.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     UTILITY
!
! $ Declarations
!
!     INTEGER               BEG
!     INTEGER               END
!     INTEGER               IDATA ( * )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BEG,
!     END        I   Begin and end addresses of range to update.
!     IDATA      I   Integer data.
!
! $ Detailed_Input
!
!     BEG,
!     END            are the first and last of a range of EK scratch
!                    area addresses to write to.  BEG and END must
!                    satisfy the relations
!
!                       1  <  BEG  <  END  <  TOP
!                          -       -       -
!
!                    where TOP is the last EK scratch area stack top
!                    at the time this routine is called.
!
!     IDATA          is an integer array containing data to write to
!                    the specified range of addresses in the EK scratch
!                    area.  The first END-BEG+1 elements of IDATA are
!                    written to the specified range in the EK scratch
!                    area, in order.
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
!     1)  If either of BEG or END are outside of the range 1:TOP,
!         where TOP is the EK scratch area stack top, the error
!         SPICE(INVALIDADDRESS) is signaled.
!
!     2)  If END < BEG, this routine simply returns.  No error
!         is signaled.
!
!     3)  If an I/O error occurs during the data addition, the error
!         will be diagnosed by routines called by this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Let TOP be the EK scratch area stack top prior to a call to this
!     routine.  This routine is used to modify values in the scratch
!     area that lie in the address range 1:TOP.
!
! $ Examples
!
!     See the header of the umbrella routine ZZEKSCA for an example
!     of use of this routine.
!
! $ Restrictions
!
!     1)  This routine must execute quickly.  Therefore, it checks in
!         only if it detects an error.  If an error is signaled by a
!         routine called by this routine, this routine will not appear
!         in the SPICELIB traceback display.  Also, in the interest
!         of speed, this routine does not test the value of the SPICELIB
!         function RETURN upon entry.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 2.0.0, 23-FEB-1995 (NJB)
!
!        Updated for EK architecture 3.
!
!-    Beta Version 1.0.0, 25-FEB-1993 (NJB)
!
!-&

! $ Index_Entries
!
!     update data in EK scratch area
!
!-&



!
!     No checking in here.
!

!
!     Validate the addresses.
!
      if (  ( beg < 1 ) .or. ( beg > t )  ) then

         call setmsg ( 'Start address BEG was #; valid range is 1:#' )
         call errint ( '#',  beg                                     )
         call errint ( '#',  t                                       )
         call sigerr ( 'SPICE(INVALIDADDRESS)'                       )
         return

      else if (  ( end < 1 ) .or. ( end > t  )  ) then

         call setmsg ( 'End address END was #; valid range is 1:#'   )
         call errint ( '#',  end                                     )
         call errint ( '#',  t                                       )
         call sigerr ( 'SPICE(INVALIDADDRESS)'                       )
         return

      else if ( beg > end ) then
         return
      end if


      if ( end <= memsiz ) then
!
!        If the entire range is in memory, fine.  Update the range
!        now.
!
         do i = beg, end
            scrtch(i)  =  idata(i-beg+1)
         end do


      else if ( beg <= memsiz ) then
!
!        Update the portion of the address range that's in memory.
!
         do i = beg, memsiz
            scrtch(i)  =  idata(i-beg+1)
         end do

!
!        Now update the rest of the range, which is in the scratch
!        DAS file.
!
         call dasudi  ( scrhan, &
                        1, &
                        end-memsiz, &
                        idata( memsiz-beg+2 )  )

      else
!
!        The whole range is in the DAS file.
!
         call dasudi ( scrhan,  beg-memsiz,  end-memsiz,  idata )

      end if

      return






!$Procedure    ZZEKSRD  ( EK scratch area, read )

      entry zzeksrd ( beg, end, idata )

! $ Abstract
!
!     Read from a range of addresses in the EK scratch area.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     UTILITY
!
! $ Declarations
!
!     INTEGER               BEG
!     INTEGER               END
!     INTEGER               IDATA ( * )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BEG,
!     END        I   Begin and end addresses of range to read from.
!     IDATA      O   Integer data.
!
! $ Detailed_Input
!
!     BEG,
!     END            are the first and last of a range of EK scratch
!                    area addresses to read from. BEG and END must
!                    satisfy the relations
!
!                       1  <  BEG  <  END  <  LAST
!                          -       -       -
!
!                    where LAST is the last EK scratch area address
!                    in use at the time this routine is called.
!
! $ Detailed_Output
!
!     IDATA          is an integer array containing data read from the
!                    range of addresses BEG:END in the EK scratch area.
!                    The first END-BEG+1 elements of IDATA are assigned
!                    in order using the contents of this address range.
!                    IDATA must have dimension at least END-BEG+1.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If either of BEG or END are outside of the range 1:LAST,
!         where LAST is the last address already in use in the EK
!         scratch area, the error SPICE(INVALIDADDRESS) is signaled.
!
!     2)  If END < BEG, this routine simply returns.  No error
!         is signaled.
!
!     3)  If an I/O error occurs during the read, the error will be
!         diagnosed by routines called by this routine.
!
!     4)  If IDATA has dimension less than END-BEG+1, the results of
!         a call to this routine will be unpredictable, except that
!         you can safely predict they'll be wrong.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Let LAST be the last address in use in the EK scratch area prior
!     to a call to this routine.  This routine is used to read values
!     in the scratch area that lie in the address range 1:LAST.
!
! $ Examples
!
!     See the header of the umbrella routine ZZEKSCA for an example
!     of use of this routine.
!
! $ Restrictions
!
!     1)  This routine must execute quickly.  Therefore, it checks in
!         only if it detects an error.  If an error is signaled by a
!         routine called by this routine, this routine will not appear
!         in the SPICELIB traceback display.  Also, in the interest
!         of speed, this routine does not test the value of the SPICELIB
!         function RETURN upon entry.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 23-FEB-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     read from EK scratch area
!
!-&

!
!     No checking in here.
!

!
!     Validate the addresses.
!
      if (  ( beg < 1 ) .or. ( beg > t  )  ) then

         call setmsg ( 'Start address BEG was #; valid range is 1:#' )
         call errint ( '#',  beg                                     )
         call errint ( '#',  t                                       )
         call sigerr ( 'SPICE(INVALIDADDRESS)'                       )
         return

      else if (  ( end < 1 ) .or. ( end > t  )  ) then

         call setmsg ( 'End address END was #; valid range is 1:#'   )
         call errint ( '#',  end                                     )
         call errint ( '#',  t                                       )
         call sigerr ( 'SPICE(INVALIDADDRESS)'                       )
         return

      else if ( beg > end ) then
         return
      end if


      if ( end <= memsiz ) then
!
!        If the entire range is in memory, fine.  Read from the range
!        now.
!
         do i = beg, end
            idata(i-beg+1)  =  scrtch(i)
         end do


      else if ( beg <= memsiz ) then
!
!        Read from the portion of the address range that's in memory.
!
         do i = beg, memsiz
            idata(i-beg+1)  =  scrtch(i)
         end do

!
!        Now read the rest of the range, which is in the scratch
!        DAS file.
!
         call dasrdi  ( scrhan, &
                        1, &
                        end-memsiz, &
                        idata( memsiz-beg+2 )  )

      else
!
!        The whole range is in the DAS file.
!
         call dasrdi ( scrhan,  beg-memsiz,  end-memsiz,  idata )

      end if

      return



!$Procedure    ZZEKSCLN  ( EK scratch area, clean up )

      entry zzekscln

! $ Abstract
!
!     Clean up:  re-initialize the EK scratch area; unload the
!     scratch file.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     UTILITY
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
!     None.
!
! $ Files
!
!     This routine unloads the scratch DAS used by this system.
!
! $ Particulars
!
!     This routine is intended to enable test software to unload
!     the scratch DAS file used by the EK scratch area routines.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) Many EK routines operate by side effects on the EK scratch
!        area, so this routine must be used with caution.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.1.0, 29-JUL-2003 (NJB)
!
!        Added DASWBR call.  This call frees the buffer records used by
!        the scratch file.
!
!-    SPICELIB Version 3.0.0, 27-DEC-2001 (NJB)
!
!-&


! $ Index_Entries
!
!     clean up EK scratch area
!
!-&

!
!     No checking in here.
!

!
!     Clean out the stack buffer.
!
      call cleari ( memsiz, scrtch )
      t  = 0

!
!     If FIRST has been set to .FALSE., we've an open scratch DAS
!     to dispose of.
!
      if ( .not. first ) then
!
!        Write out the buffered records belonging to the scratch file;
!        this will cause them to be returned to the free list.
!
         call daswbr ( scrhan )

!
!        Dump the scratch DAS.
!
         call dasllc ( scrhan )

      end if

!
!     Tell the system to re-initialize on the next pass.
!
      first = .true.

      return
      end
