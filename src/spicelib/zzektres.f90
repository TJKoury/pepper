!$Procedure  ZZEKTRES ( Private: EK, resolve times in encoded query )

      subroutine zzektres ( query,  eqryi,  eqryc,   eqryd, &
                                    error,  errmsg,  errptr  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Resolve time values in an encoded EK query.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      include 'ekattdsc.inc'
      include 'ekbool.inc'
      include 'ekcnamsz.inc'
      include 'ekopcd.inc'
      include 'ekqlimit.inc'
      include 'ekquery.inc'
      include 'ektnamsz.inc'
      include 'ektype.inc'

      integer               lbcell
      parameter           ( lbcell = -5 )

      character*(*)         query
      integer               eqryi  ( lbcell : * )
      character*(*)         eqryc
      double precision      eqryd  ( * )
      logical               error
      character*(*)         errmsg
      integer               errptr

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI     I-O  Integer component of query.
!     EQRYC     I-O  Character component of query.
!     EQRYD     I-O  Numeric component of query.
!     ERROR      O   Error flag.
!     ERRMSG     O   Error message.
!     ERRPTR     O   Position in query where error was detected.
!
! $ Detailed_Input
!
!     QUERY          is the original query from which EQRYI and EQRYC
!                    were obtained.  QUERY is used only for
!                    construction of error messages.
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed.
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     EQRYD          is the numeric portion of an encoded EK query.
!
! $ Detailed_Output
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    On output, all valid time values will have been
!                    converted from strings to equivalent numeric
!                    values which represent times as ephemeris
!                    seconds past J2000 (TDB).
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     ERROR          is a logical flag indicating whether an error was
!                    detected.  Note that a time string might be
!                    syntactically valid, but incapable of being
!                    converted to ET if the appropriate time kernels
!                    (Leapseconds or SCLK) are not loaded.
!
!     ERRMSG         is an error message describing an error in the
!                    input query, if one was detected.  If ERROR is
!                    returned .FALSE., then ERRPTR is undefined.
!
!     ERRPTR         is the character position in the original query
!                    at which an error was detected, if an error was
!                    found.  This index refers to the offending lexeme's
!                    position in the original query represented by the
!                    input encoded query.  If ERROR is returned .FALSE.,
!                    ERRPTR is undefined.
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input query is not initialized, the error will be
!         diagnosed by routines called by this routine.  The outputs
!         will not be modified.
!
!     2)  If names have not been resolved in the input query, the error
!         SPICE(NAMESNOTRESOLVED) will be signaled.  The outputs
!         will not be modified.
!
!     3)  If any sort of time conversion error occurs, the output flag
!         ERROR is set, and an error message is returned.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Strings representing time values are interpreted as follows:
!
!        1)  The string is first examined to see whether it's an
!            SCLK string for a recognized clock; if it is, the
!            string is converted to the equivalent ET.
!
!        2)  If the string is not a SCLK string, it is expected
!            to be some sort of UTC representation.  The string is
!            checked to see whether it's an ISO format UTC time that
!            ISO2UTC can handle.
!
!        3)  If the string does not conform to an ISO format, the
!            last chance is to try to get the string through
!            TPARSE.  If TPARSE can't deal with it, it's considered
!            to be invalid.
!
!
!     This routine assumes that encoded EK query architecture version
!     1 is to be used with the query to be initialized; this routine
!     will not work with any other architecture version.
!
! $ Examples
!
!     See EKFIND.
!
! $ Restrictions
!
!     1) A leapseconds kernel must be loaded at the time this routine
!        is called.
!
!     2) In order to convert SCLK strings, an appropriate SCLK kernel
!        must be loaded at the time this routine is called.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               failed

!
!     Local parameters
!
      integer               short
      parameter           ( short  = 32 )

!
!     Local variables
!
      character*(tnamsz)    alias  ( maxtab )
      character*(cnamsz)    colnam
      character*(tnamsz)    table  ( maxtab )
      character*(short)     timstr
      character*(1)         touchc

      double precision      et

      integer               attdsc ( adscsz )
      integer               base
      integer               cnstyp
      integer               colidx
      integer               descr  ( eqvdsz )
      integer               dtype
      integer               i
      integer               irsolv
      integer               lxb
      integer               lxe
      integer               ncns
      integer               ntab
      integer               opcode
      integer               sb
      integer               se
      integer               tabidx

!
!     No error to start with.
!
      error  =  .false.
      errmsg =  ' '
      errptr =  0

      touchc =  query(1:1)

!
!     The query must have had names resolved at this point, or it's no
!     go.
!
      call zzekreqi ( eqryi, 'NAMES_RESOLVED', irsolv )

      if ( failed() ) then
         return
      end if

      if ( irsolv .eq. ifalse ) then

         call chkin  ( 'ZZEKTRES'                                  )
         call setmsg ( 'Names are not resolved in encoded query.'  )
         call sigerr ( 'SPICE(NAMESNOTRESOLVED)'                   )
         call chkout ( 'ZZEKTRES'                                  )
         return

      end if

!
!     Time strings occur only on the right sides of constraints.
!     Examine each constraint that compares a column and a value.
!
      call zzekreqi ( eqryi, 'NUM_TABLES',      ntab )
      call zzekreqi ( eqryi, 'NUM_CONSTRAINTS', ncns )

      do i = 1, ncns
!
!        Calculate the base address of the constraint.
!
         base    =  eqvbas + (ntab * 2 * eqvdsz) + (i-1)*eqcdsz

!
!        Obtain the constraint type.  If the RHS is not a value or if
!        the RHS is null (as indicated by the opcode), we can skip it.
!
         cnstyp  =  eqryi ( base + eqctyp )
         opcode  =  eqryi ( base + eqopcd )

         if (        ( cnstyp .eq. eqval  ) &
              .and.  ( opcode .ne. isnull ) &
              .and.  ( opcode .ne. notnul )  ) then
!
!           Get the index of the table containing the LHS column, and
!           get the index of this column within that table.  Get the
!           table name, then get the column's attributes.
!
            tabidx  =  eqryi ( base + eqltab - 1 + eqtord )
            colidx  =  eqryi ( base + eqlcol - 1 + eqcidx )

            call zzekqtab ( eqryi, eqryc, tabidx, table, alias )

            call ekcii ( table, colidx, colnam, attdsc )

            dtype  =  attdsc(2)


            if ( dtype .eq. time ) then
!
!              The RHS points to a string representing a time
!              value.
!
               lxb    =  eqryi ( base + eqbval - 1 + eqblex )
               lxe    =  eqryi ( base + eqbval - 1 + eqblex )
               sb     =  eqryi ( base + eqbval - 1 + eqbstr )
               se     =  eqryi ( base + eqbval - 1 + eqestr )
               timstr =  eqryc ( sb:se )

!
!              Convert the time to ET, if possible.
!
               call zzektcnv ( timstr, et, error, errmsg )

               if ( error ) then

                  errptr = sb
                  return

               end if
!
!              Insert the ET value into the query, and replace the
!              value descriptor for the time string.
!
               call zzekinqn ( et, time, lxb, lxe, eqryi, eqryd, descr )

               call movei ( descr, eqvdsz, eqryi(base+eqbval) )

            end if
!
!           We've parsed a time string, if the current column's type
!           was TIME.
!

         end if
!
!        We've examined the current constraint, if it compares a
!        column with a value.
!

      end do

!
!     Indicate completion of time resolution.
!
      call zzekweqi (  'TIMES_RESOLVED',  itrue,  eqryi  )


      return
      end
