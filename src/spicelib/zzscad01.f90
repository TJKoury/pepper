!$Procedure ZZSCAD01 ( SPICE private, add clock to database, type 01 )

      SUBROUTINE ZZSCAD01 ( SC,     HDSCLK, SCPOOL, CLKLST, DPFREE, &
                            DPBUFF, IFREE,  INTBUF, SCBASE, SCLKAT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Add to the SCLK type 01 database data for a specified SCLK.
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
!     SCLK
!
! $ Keywords
!
!     SCLK
!     TIME
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'errhnd.inc'
      INCLUDE 'zzsc01.inc'

      INTEGER               SC
      INTEGER               HDSCLK ( * )
      INTEGER               SCPOOL ( LBSNGL : * )
      INTEGER               CLKLST ( * )
      INTEGER               DPFREE
      DOUBLE PRECISION      DPBUFF ( * )
      INTEGER               IFREE
      INTEGER               INTBUF ( * )
      INTEGER               SCBASE ( * )
      INTEGER               SCLKAT

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     SC         I   SCLK ID code.
!     HDSCLK    I-O  Array of heads of hash collision lists.
!     SCPOOL    I-O  Singly linked collision list pool.
!     CLKLST    I-O  Array of SCLK codes.
!     DPFREE    I-O  Index of first free entry in d.p. data buffer.
!     DPBUFF    I-O  D.p. (double precision) data buffer
!     IFREE     I-O  Index of first free entry in integer data buffer.
!     INTBUF    I-O  Integer data buffer.
!     SCBASE    I-O  Base index in integer buffer corresponding to SC.
!     SCLKAT     O   Index in CLKLST of SC.
!
! $ Detailed_Input
!
!     SC          is the SCLK ID of a type 1 clock. Data for this clock
!                 are to be added to the type 1 database.
!
!     HDSCLK      is an array containing the head node indices of the
!                 SCLK ID collision lists stored in SCPOOL.
!
!     SCPOOL      is a singly linked list pool containing collision
!                 lists for sets of SCLK IDs that have the same hash
!                 values.
!
!     CLKLST      is an array of SCLK IDs, which is parallel to the
!                 data portion of SCPOOL. Each allocated node of
!                 SCPOOL is the index in CLKLST of the associated
!                 SCLK ID.
!
!     DPFREE      is index in the database's double precision buffer of
!                 the first free element.
!
!     DPBUFF      is the double precision buffer of the type 1
!                 database.
!
!     IFREE       is index in the database's integer buffer of the
!                 first free element.
!
!     INTBUF      is the integer buffer of the type 1 database.
!
!     SCBASE      is an array of base addresses in the array INTBUF of
!                 the integer data sets associated with the clocks
!                 in the type 1 database. The Ith element of SCBASE is
!                 the base address of the clock at index I in the
!                 array CLKLST.
!
!                 The "base" address of an integer data set immediately
!                 precedes the first index of that data set: the index
!                 in INTBUF of the first element of the integer data of
!                 CLKLST(I) is SCBASE(I)+1.
!
! $ Detailed_Output
!
!     HDSCLK      is the input list head array, updated as needed to
!                 reflect the addition of the new clock. Note that the
!                 array won't be updated if the clock is added to a
!                 pre-existing collision list.
!
!     SCPOOL      is the input singly linked list pool, updated to
!                 reflect the addition of the new clock.
!
!     CLKLST      is an array of SCLK IDs, updated to reflect the
!                 addition of the new clock.
!
!     DPFREE      is index in the database's double precision buffer of
!                 the first free element after addition of data for the
!                 new clock.
!
!     DPBUFF      is the double precision buffer of the type 1
!                 database, updated to reflect the addition of the new
!                 clock.
!
!     IFREE       is index in the database's integer buffer of the
!                 first free element after addition of data for the new
!                 clock.
!
!     INTBUF      is the integer buffer of the type 1 database, updated
!                 to reflect the addition of the new clock.
!
!     SCBASE      is an array of base addresses in the array INTBUF,
!                 updated to reflect the addition of the new clock.
!
!     SCLKAT      is the index in CLKLST at which the input clock ID
!                 SC is stored.
!
! $ Parameters
!
!     See the include files zzsc01.inc and zzctr.inc.
!
! $ Exceptions
!
!
!     1)  If any of the kernel variables containing SCLK coefficients,
!         partition start or stop times, moduli, or offsets are not
!         found, the error SPICE(KERNELVARNOTFOUND) will be signaled.
!
!     2)  If any integer kernel variables are not found, the error will
!         be signaled by a routine in the call tree of this routine.
!
!     3)  If any kernel variable used by this routine does not have
!         numeric type, the error will be signaled by a routine in the
!         call tree of this routine.
!
!     4)  If the counts of partition start or stop times do not match,
!         the error SPICE(NUMPARTSUNEQUAL) will be signaled.
!
!     5)  If the count of partitions exceeds the limit MXPART,
!         the error SPICE(TOOMANYPARTITIONS) will be signaled.
!
!     6)  If the count of coefficients exceeds the limit 3*MXCOEF,
!         the error SPICE(TOOMANYCOEFFS) will be signaled.
!
!     7)  If any kernel variable other than the coefficient set or
!         the partition bounds has size that exceeds the applicable
!         limit, the error SPICE(KERNELVARTOOLARGE) will be signaled.
!
!     8)  If an error occurs while this routine looks up data from the
!         kernel pool, the error will be signaled by a routine in the
!         call tree of this routine.
!
!     9)  If an error occurs while this routine adds data to a hash,
!         the error will be signaled by a routine in the call tree of
!         this routine.
!
!     10) If the database contains entries for the maximum number
!         of clocks and an addition is requested, the database will
!         be re-initialized, and an entry for the new clock will be
!         created. This case is not an error.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!        - An SCLK kernel providing data for the SCLK designated by
!          the input ID code SC.
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     None. See usage in ZZSCUP01.
!
! $ Restrictions
!
!     This is a SPICE-private routine. It should not be called directly
!     by user application code.
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
!-    SPICELIB Version 1.0.0, 01-DEC-2021 (NJB)
!
!-&

! $ Index_Entries
!
!     add sclk to type 1 sclk database
!
!-&


! $ Revisions
!
!     None.
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN =     60 )

!
!     Following are parameters for the indices within the
!     array NAMLST of the kernel variable names used by the
!     SC01 entry points.
!
!     NOTE: indices for the d.p. variables must be listed first.
!
      INTEGER               COFIDX
      PARAMETER           ( COFIDX =          1 )

      INTEGER               PRSIDX
      PARAMETER           ( PRSIDX = COFIDX + 1 )

      INTEGER               PREIDX
      PARAMETER           ( PREIDX = PRSIDX + 1 )

      INTEGER               OFFIDX
      PARAMETER           ( OFFIDX = PREIDX + 1 )

      INTEGER               MODIDX
      PARAMETER           ( MODIDX = OFFIDX + 1 )

      INTEGER               NFLIDX
      PARAMETER           ( NFLIDX = MODIDX + 1 )

      INTEGER               DELIDX
      PARAMETER           ( DELIDX = NFLIDX + 1 )

      INTEGER               SYSIDX
      PARAMETER           ( SYSIDX = DELIDX + 1 )

      INTEGER               NKITEM
      PARAMETER           ( NKITEM = SYSIDX )

      INTEGER               NDP
      PARAMETER           ( NDP    = MODIDX )

!
!     Maximum number of elements in coefficient array:
!
      INTEGER               MXCFSZ
      PARAMETER           ( MXCFSZ =  3 * MXCOEF )

!
!     Local variables
!
      CHARACTER*(1)         KVTYPE ( NKITEM )
      CHARACTER*(NAMLEN)    KVNAME ( NKITEM )
      CHARACTER*(NAMLEN)    NAMLST ( NKITEM )
      CHARACTER*(SMSGLN)    SHRTMS

      INTEGER               DPBASE
      INTEGER               DPROOM
      INTEGER               I
      INTEGER               IBASE
      INTEGER               IBIX   ( NKITEM )
      INTEGER               IROOM
      INTEGER               KVMAXN ( NKITEM )
      INTEGER               KVSIZE ( NKITEM )
      INTEGER               N
      INTEGER               NCOEFF
      INTEGER               NDPVAL
      INTEGER               NMOD
      INTEGER               NOFF
      INTEGER               NPART
      INTEGER               NPARTB
      INTEGER               NPARTE
      INTEGER               NSCAVL
      INTEGER               NSYS
      INTEGER               PRVSC

      LOGICAL               FOUND
      LOGICAL               NEW

!
!     Saved values
!
      SAVE                 IBIX
      SAVE                 KVMAXN
      SAVE                 NAMLST

!
!     Initial values
!
      DATA ( NAMLST(I), KVMAXN(I), IBIX(I), I = 1, NKITEM ) / &
        'SCLK01_COEFFICIENTS',    MXCFSZ,    IXBCOF, &
        'SCLK_PARTITION_START',   MXPART,    IXBSTR, &
        'SCLK_PARTITION_END',     MXPART,    IXBEND, &
        'SCLK01_OFFSETS',         MXNFLD,    IXBOFF, &
        'SCLK01_MODULI',          MXNFLD,    IXBMOD, &
        'SCLK01_N_FIELDS',        1,         0, &
        'SCLK01_OUTPUT_DELIM',    1,         0, &
        'SCLK01_TIME_SYSTEM',     1,         0              /


      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSCAD01' )

!
!     Make up a list of names of kernel variables that we'll use.
!     The first name in the list is SCLK_KERNEL_ID, which does not
!     require the addition of a spacecraft code suffix.  For the
!     rest of the names, we'll have to add the suffix.
!
      CALL MOVEC ( NAMLST, NKITEM, KVNAME )

      DO I = 1, NKITEM
         CALL SUFFIX ( '_#',        0,         KVNAME(I) )
         CALL REPMI  ( KVNAME(I),  '#',  -SC,  KVNAME(I) )
      END DO

!
!     Check available room in the clock hash and in the numeric data
!     buffers. If there's no room, re-initialize the entire data
!     structure.
!
      CALL ZZHSIAVL ( SCPOOL, NSCAVL )

      IF ( NSCAVL .EQ. 0 ) THEN

         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )

      END IF

!
!     Add this clock to the hash.
!
      CALL ZZHSIADD ( HDSCLK, SCPOOL, CLKLST, SC, SCLKAT, NEW )

      IF ( FAILED() ) THEN
!
!        This code should be unreachable but is provided for safety.
!
         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )
         CALL CHKOUT ( 'ZZSCAD01' )
         RETURN

      END IF

!
!     Check room in the integer buffer.
!
      IROOM = IBUFSZ + 1 - IFREE

      IF (  ( IROOM .LT. NIVALS ) .OR. ( IROOM .GT. IBUFSZ )  ) THEN
!
!        This code should be unreachable but is provided for safety.
!
         I = IFREE

         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )

         CALL SETMSG ( 'IROOM was #; must be in range #:#. ' &
         //            'IFREE was #; must be in range 1:#.' )
         CALL ERRINT ( '#', IROOM    )
         CALL ERRINT ( '#', NIVALS   )
         CALL ERRINT ( '#', IBUFSZ   )
         CALL ERRINT ( '#', I        )
         CALL ERRINT ( '#', IBUFSZ+1 )
         CALL SIGERR ( 'SPICE(BUG)'  )
         CALL CHKOUT ( 'ZZSCAD01'    )
         RETURN

      END IF

!
!     Compute available room for double precision data.
!
      DPROOM = DBUFSZ + 1 - DPFREE

      IF (  ( DPROOM .LT. 0 ) .OR. ( DPROOM .GT. DBUFSZ )  ) THEN
!
!        This code should be unreachable but is provided for safety.
!
         I = DPFREE

         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )

         CALL SETMSG ( 'DPROOM was #; must be in range 0:#. ' &
         //            'DPFREE was #; must be in range 1:#.' )
         CALL ERRINT ( '#', DPROOM   )
         CALL ERRINT ( '#', DBUFSZ   )
         CALL ERRINT ( '#', I        )
         CALL ERRINT ( '#', DBUFSZ+1 )
         CALL SIGERR ( 'SPICE(BUG)'  )
         CALL CHKOUT ( 'ZZSCAD01'    )
         RETURN

      END IF

!
!     Set the base address for the next item to be added to the
!     integer buffer. The index of the first element of the item
!     is the successor of IBASE.
!
      IBASE            = IFREE-1
      SCBASE( SCLKAT ) = IBASE

!
!     Fetch from the kernel pool the integer items we need:
!
!        -  The number of fields in an (unabridged) SCLK string
!        -  The output delimiter code
!        -  The parallel time system code (optional; defaults to TDB)
!
      CALL SCLI01 ( NAMLST(NFLIDX), SC, 1, N,    INTBUF(IBASE+IXNFLD) )
      CALL SCLI01 ( NAMLST(DELIDX), SC, 1, N,    INTBUF(IBASE+IXDLIM) )
      CALL SCLI01 ( NAMLST(SYSIDX), SC, 1, NSYS, INTBUF(IBASE+IXTSYS) )

      IF ( FAILED() ) THEN

         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )
         CALL CHKOUT ( 'ZZSCAD01' )
         RETURN

      END IF

!
!     The time system code need not be provided in the kernel pool.
!     Set it to the default value if it's not present.
!
      IF ( NSYS .EQ. 0 ) THEN
         INTBUF(IBASE+IXTSYS) = TDB
      END IF

!
!     Check for presence in the kernel pool of all required d.p. kernel
!     variables.
!
!     Get and check the sizes of the d.p. arrays: coefficients,
!     partitions, moduli, and offsets.
!
!     We obtain sizes of d.p. kernel variables rather than using sizes
!     returned by SCLD01 because we'll read data directly into our
!     passed-in buffers. We need to know in advance how much room is
!     needed.
!
      DO I = 1, NDP

         CALL DTPOOL ( KVNAME(I), FOUND, KVSIZE(I), KVTYPE(I) )

         IF ( FAILED() ) THEN
!
!           This code should be unreachable but is provided for
!           safety. Initialize local data structures. ZZSCIN01 sets
!           PRVSC to 0.
!
            CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, &
                            DPFREE, IFREE,  PRVSC  )

            CALL CHKOUT ( 'ZZSCAD01' )
            RETURN

         END IF

!
!        Check that each required d.p. kernel variable was found.
!
         IF ( .NOT. FOUND ) THEN

            CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, &
                            DPFREE, IFREE,  PRVSC  )

            CALL SETMSG ( 'Kernel variable # for spacecraft clock ' &
            //            '# was not found. An SCLK kernel for ' &
            //            'this clock may not have been loaded.'   )
            CALL ERRCH  ( '#', KVNAME(I)                           )
            CALL ERRINT ( '#', SC                                  )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'               )
            CALL CHKOUT ( 'ZZSCAD01'                               )
            RETURN

         END IF

!
!        Check the number of values associated with the kernel
!        variable.
!
         IF ( KVSIZE(I) .GT. KVMAXN(I) ) THEN

            CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, &
                            DPFREE, IFREE,  PRVSC  )

            CALL SETMSG ( 'The number of values associated with ' &
            //            'the kernel variable # for clock # ' &
            //            'is #, which exceeds the limit #.'     )
            CALL ERRCH  ( '#', KVNAME(I)                         )
            CALL ERRINT ( '#', SC                                )
            CALL ERRINT ( '#', KVSIZE(I)                         )
            CALL ERRINT ( '#', KVMAXN(I)                         )
!
!           Customize the short error message a bit.
!
            IF ( I .EQ. COFIDX ) THEN

               SHRTMS = 'SPICE(TOOMANYCOEFFS)'

            ELSE IF (     ( I .EQ. PRSIDX ) &
                     .OR. ( I .EQ. PREIDX ) ) THEN

               SHRTMS = 'SPICE(TOOMANYPARTITIONS)'
            ELSE
               SHRTMS = 'SPICE(KERNELVARTOOLARGE)'
            END IF

            CALL SIGERR (  SHRTMS    )
            CALL CHKOUT ( 'ZZSCAD01' )
            RETURN

         END IF

      END DO

!
!     Store kernel variable sizes as scalars for convenience.
!
      NCOEFF = KVSIZE(COFIDX)
      NPARTB = KVSIZE(PRSIDX)
      NPARTE = KVSIZE(PREIDX)
      NMOD   = KVSIZE(MODIDX)
      NOFF   = KVSIZE(OFFIDX)

!
!     NDPVAL is the number of d.p. values that must be buffered to
!     support this clock.
!
      NDPVAL = NCOEFF + NPARTB + NPARTE + NMOD + NOFF

!
!     Check whether sizes of partition start and end kernel variables
!     match.
!
      IF ( NPARTB .NE. NPARTE  ) THEN

         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )

         CALL SETMSG ( 'The numbers of partition start times # and ' &
         //            'stop times # are unequal for spacecraft ' &
         //            'clock #.'                                  )
         CALL ERRINT ( '#', NPARTB                                 )
         CALL ERRINT ( '#', NPARTE                                 )
         CALL ERRINT ( '#', SC                                     )
         CALL SIGERR ( 'SPICE(NUMPARTSUNEQUAL)'                    )
         CALL CHKOUT ( 'ZZSCAD01'                                  )
         RETURN

      ELSE
         NPART = NPARTB
      END IF

      IF ( NDPVAL .GT. DBUFSZ ) THEN
!
!        We couldn't make enough room even if we dumped all buffered
!        data.
!
!        This code should be unreachable but is provided for safety.
!        We've already checked the existence, types, and sizes of the
!        d.p. kernel variables.
!
         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )

         CALL SETMSG ( 'Total number of double precision data ' &
         //            'values for SCLK # is #; this count exceeds ' &
         //            'the maximum supported count #.' )
         CALL ERRINT ( '#', SC                          )
         CALL ERRINT ( '#', NDPVAL                      )
         CALL ERRINT ( '#', DBUFSZ                      )
         CALL SIGERR ( 'SPICE(BUG)'                     )
         CALL CHKOUT ( 'ZZSCAD01'                       )
         RETURN

      ELSE IF ( NDPVAL .GT. DPROOM ) THEN
!
!        We don't have room for this clock, but we will if we dump
!        the existing buffered data.
!
         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )

      END IF

!
!     Store in the integer buffer the sizes of the coefficient array
!     and the number of partitions.
!
      INTBUF( IBASE+IXNCOF ) = NCOEFF
      INTBUF( IBASE+IXNPRT ) = NPART

!
!     Fetch from the kernel pool the d.p. items we need:
!
!        -  The SCLK coefficients array
!        -  The partition start times
!        -  The partition end times
!        -  The moduli of the fields of an SCLK string
!        -  The offsets for each clock field.
!
!     Set the base index in the d.p. buffer of the next d.p. item
!     to store. The first element of the item has index DPBASE+1.
!
!     We don't update DPFREE until all d.p. data have been fetched
!     successfully.
!
      DPBASE = DPFREE - 1

      DO I = 1, NDP
!
!        Store in the integer buffer the base index in the d.p.
!        buffer of the kernel variable's values.
!
         INTBUF( IBASE + IBIX(I) ) = DPBASE
!
!        Fetch d.p. values into buffer.
!
         CALL SCLD01 ( NAMLST(I), SC, &
                       KVMAXN(I), N,  DPBUFF(DPBASE + 1) )

         IF ( FAILED() ) THEN
!
!           This code could be reached if the kernel variable has
!           character type.
!
            CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, &
                            DPFREE, IFREE,  PRVSC  )

            CALL CHKOUT ( 'ZZSCAD01' )
            RETURN

         END IF
!
!        Update the d.p. buffer base index.
!
         DPBASE = DPBASE + N

      END DO

!
!     Account for buffer usage for the new clock.
!
      DPFREE = DPFREE + NDPVAL
      IFREE  = IFREE  + NIVALS

      CALL CHKOUT ( 'ZZSCAD01' )
      RETURN
      END
