!$Procedure  ZZNOFCON ( Create frame connection long error message )

      subroutine zznofcon ( et, frame1, endp1, frame2, endp2, errmsg )
      use pepper_kinds, only: ip, wp

      implicit none

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Create an informative long error message for cases where the
!     frame system signals a SPICE(NOFRAMECONNECT) error.
!
! $ Required_Reading
!
!     FRAMES
!     TIME
!
! $ Keywords
!
!     FRAMES
!     PRIVATE
!     UTILITY
!
! $ Declarations

      include 'frmtyp.inc'

      real(wp)              et
      integer(ip)           frame1
      integer(ip)           endp1
      integer(ip)           frame2
      integer(ip)           endp2
      character*(*)         errmsg

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!     FRAME1     I   "From" frame ID code.
!     ENDP1      I   "From" path endpoint frame ID code.
!     FRAME2     I   "To" frame ID code.
!     ENDP2      I   "To" path endpoint frame ID code.
!     ERRMSG     O   Long error message.
!
! $ Detailed_Input
!
!     ET             Epoch of frame transformation, expressed as
!                    seconds past J2000 TDB.
!
!     FRAME1         Frame ID code of frame at start of first path.
!
!     ENDP1          Frame ID code of frame at end of first path;
!                    this frame is the last node that could be
!                    reached from the frame designated by FRAME1.
!
!     FRAME2         Frame ID code of frame at start of second path.
!
!     ENDP2          Frame ID code of frame at end of second path;
!                    this frame is the last node that could be
!                    reached from the frame designated by FRAME2.
!
! $ Detailed_Output
!
!     ERRMSG         Long error message specifying computable
!                    frame paths, indications of missing SCLK
!                    or CK data, and optionally, debugging hints.
!
!                    The rules for formation of this message are:
!
!                       1) State the epoch.
!
!                       2) State the names of the frames for which
!                          a connection was attempted, if these
!                          names are available.
!
!                       3) State the names of the frames at the
!                          endpoints of both paths, if these
!                          names are available.
!
!                          Omit this portion of the message for any
!                          path of length one: in other words, if a
!                          frame and path endpoint coincide, omit the
!                          clause stating the frame can be connected to
!                          itself.
!
!                       4) For any path endpoint frame, if that
!                          frame is of CK type, indicate that
!                          CK and SCLK data must be loaded for
!                          that frame.
!
!                       5) For any path endpoint frame, if that
!                          frame is of CK type and SCLK data for the
!                          SCLK associated with that frame are not
!                          available, indicate this problem, along with
!                          the CK and SCLK ID codes associated with
!                          this frame.
!
!                       6) If at least one path endpoint frame
!                          is of CK type, and all required SCLK data
!                          are present, include a closing message
!                          explaining how CK coverage may be inadequate
!                          and recommending use of CKBRIEF.
!
!                       7) If both path endpoint frames are of CK type,
!                          and required SCLK data are present for only
!                          one of these frames, include a closing
!                          message explaining how CK coverage may be
!                          inadequate for a frame for which SCLK data
!                          are available, and recommending use of
!                          CKBRIEF.
!
!                       8) Omit the closing message if no path
!                          endpoint CK frame has associated SCLK
!                          data.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If a call to FRINFO or NAMFRM signals an error, this routine
!        will not be able to create a long error message. The
!        caller will not be able to diagnose the frame connection
!        failure, since an error condition will already exist.
!
! $ Files
!
!     1) Each input frame ID argument will be mapped, if possible,
!        to a frame name. Any input frame ID that's not built in
!        must
!
! $ Particulars
!
!     This routine centralizes creation of a long error message for
!     frame connection failures. This routine should be called
!     from:
!
!        FRMCHG
!        REFCHG
!        ZZFRMCH0
!        ZZFRMCH1
!        ZZREFCH0
!        ZZREFCH1
!
! $ Examples
!
!     Below are some examples of messages created by this routine.
!
!
!    1)   At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000
!         TDB), there is insufficient information available to
!         transform from reference frame -82000 (CASSINI_SC_COORD) to
!         reference frame -41000 (MEX_SPACECRAFT). CASSINI_SC_COORD is
!         a CK frame; a CK file containing data for instrument or
!         structure -82000 at the epoch shown above, as well as a
!         corresponding SCLK kernel, must be loaded in order to use
!         this frame. Frame MEX_SPACECRAFT could be transformed to
!         frame -41001 (MEX_SC_REF). The latter is a CK frame; a CK
!         file containing data for instrument or structure -41001 at
!         the epoch shown above, as well as a corresponding SCLK
!         kernel, must be loaded in order to use this frame. Failure to
!         find required CK data could be due to one or more CK files
!         not having been loaded, or to the epoch shown above lying
!         within a coverage gap or beyond the coverage bounds of the
!         loaded CK files. It is also possible that no loaded CK file
!         has required angular velocity data for the input epoch, even
!         if a loaded CK does have attitude data for that epoch. You
!         can use CKBRIEF with the -dump option to display coverage
!         intervals of a CK file.
!
!
!    2)   At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000
!         TDB), there is insufficient information available to
!         transform from reference frame -82360 (CASSINI_ISS_NAC) to
!         reference frame 1 (J2000). Frame CASSINI_ISS_NAC could be
!         transformed to frame -82000 (CASSINI_SC_COORD). The latter is
!         a CK frame; a CK file containing data for instrument or
!         structure -82000 at the epoch shown above, as well as a
!         corresponding SCLK kernel, must be loaded in order to use
!         this frame. Failure to find required CK data could be due to
!         one or more CK files not having been loaded, or to the epoch
!         shown above lying within a coverage gap or beyond the
!         coverage bounds of the loaded CK files. It is also possible
!         that no loaded CK file has required angular velocity data for
!         the input epoch, even if a loaded CK does have attitude data
!         for that epoch. You can use CKBRIEF with the -dump option to
!
!
!     3)  At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000
!         TDB), there is insufficient information available to
!         transform from reference frame -82000 (CASSINI_SC_COORD) to
!         reference frame -41000 (MEX_SPACECRAFT). CASSINI_SC_COORD is
!         a CK frame; a CK file containing data for instrument or
!         structure -82000 at the epoch shown above, as well as a
!         corresponding SCLK kernel, must be loaded in order to use
!         this frame. No SCLK kernel for instrument or structure
!         -82000, with corresponding SCLK ID -82, is currently loaded.
!         Frame MEX_SPACECRAFT could be transformed to frame -41001
!         (MEX_SC_REF). The latter is a CK frame; a CK file containing
!         data for instrument or structure -41001 at the epoch shown
!         above, as well as a corresponding SCLK kernel, must be loaded
!         in order to use this frame. No SCLK kernel for instrument or
!         structure -41001, with corresponding SCLK ID -41, is
!         currently loaded.
!
!
!     4)  At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000
!         TDB), there is insufficient information available to
!         transform from reference frame -82000 (CASSINI_SC_COORD) to
!         reference frame -41000 (MEX_SPACECRAFT). CASSINI_SC_COORD is
!         a CK frame; a CK file containing data for instrument or
!         structure -82000 at the epoch shown above, as well as a
!         corresponding SCLK kernel, must be loaded in order to use
!         this frame. Frame MEX_SPACECRAFT could be transformed to
!         frame -41001 (MEX_SC_REF). The latter is a CK frame; a CK
!         file containing data for instrument or structure -41001 at
!         the epoch shown above, as well as a corresponding SCLK
!         kernel, must be loaded in order to use this frame. No SCLK
!         kernel for instrument or structure -41001, with corresponding
!         SCLK ID -41, is currently loaded. For a CK frame for which
!         the corresponding SCLK kernel has been loaded, failure to
!         find required CK data could be due to one or more CK files
!         not having been loaded, or to the epoch shown above lying
!         within a coverage gap or beyond the coverage bounds of the
!         loaded CK files. It is also possible that no loaded CK file
!         has required angular velocity data for the input epoch, even
!         if a loaded CK does have attitude data for that epoch. You
!         can use CKBRIEF with the -dump option to display coverage
!         intervals of a CK file.
!
! $ Restrictions
!
!     1) This is a private routine. SPICE user applications should not
!        call this routine.
!
!     2) See exception (1) above.
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
!-    SPICELIB Version 2.0.0, 09-SEP-2013 (NJB)
!
!        Long error message for missing CK data now
!        mentions the possibility of missing angular
!        velocity data.
!
!-    SPICELIB Version 1.0.0, 14-DEC-2008 (NJB)
!
!-&

! $ Index_Entries
!
!     create error message for frame connection failure
!
!-&


!
!     SPICELIB functions
!
      logical               failed
      logical               return
      logical               zzsclk

!
!     Local parameters
!
      integer(ip)           frnmln
      parameter           ( frnmln = 32 )

      integer(ip)           timlen
      parameter           ( timlen = 35 )

      integer(ip)           phrsln
      parameter           ( phrsln = 600 )

!
!     Local variables
!
      character*(frnmln)    bname ( 2 )
      character*(frnmln)    name  ( 2 )
      character*(phrsln)    phrase
      character*(timlen)    timstr

      integer(ip)           endps  ( 2 )
      integer(ip)           center
      integer(ip)           class
      integer(ip)           clssid
      integer(ip)           frames ( 2 )
      integer(ip)           i
      integer(ip)           sclkid

      logical               ckmiss
      logical               found
      logical               havnam ( 2 )
      logical               scmiss

!
!     Because this routine might cause a SPICE error to be
!     signaled, we have to check in.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'ZZNOFCON' )

!
!     Capture input IDs in arrays.
!
      frames(1) = frame1
      frames(2) = frame2
      endps(1)  = endp1
      endps(2)  = endp2

!
!     The flags CKMISS and SCMISS are used, respectively, to
!     record whether any CK lookup failed due to missing CK
!     data or missing SCLK data. Each of these flags is turned
!     on if at least one lookup failed due to the indicated
!     cause.
!
      ckmiss = .false.
      scmiss = .false.

!
!     Get a string representation of the transformation epoch.
!
      call etcal ( et, timstr )

!
!     Get the names of the participating frames, if available.
!
      call frmnam ( frames(1),   name(1) )
      call frmnam ( frames(2),   name(2) )
      call frmnam ( endps(1),    bname(1) )
      call frmnam ( endps(2),    bname(2) )

      if ( failed() ) then
         call chkout ( 'ZZNOFCON' )
         return
      end if

      do i = 1, 2

         if ( name(i) .eq. ' ' ) then

            name(i)   = 'Name not available'
            havnam(i) = .false.
         else
            havnam(i) = .true.
         end if

         if ( bname(i) .eq. ' ' ) then
            bname(i) = 'Name not available'
         end if

      end do

      errmsg = 'At epoch # TDB (# TDB), there is ' &
      //       'insufficient information available to ' &
      //       'transform from reference frame # (@) to ' &
      //       'reference frame # (@).'


      call repmf ( errmsg, '#', et,     14, 'E', errmsg )
      call repmc ( errmsg, '#', timstr,          errmsg )

      do i = 1, 2

         call repmi ( errmsg, '#', frames(i), errmsg )
         call repmc ( errmsg, '@', name(i),   errmsg )

      end do

!
!     For any frame graph longer than a single point, tell the user
!     the endpoint of the frame connection graph originating
!     at that frame.
!
      do i = 1, 2

         if ( frames(i) .ne. endps(i)  ) then

            phrase = 'Frame # could be transformed to frame # (@).'

            if ( havnam(i) ) then
               call repmc ( phrase, '#', name(i),   phrase )
            else
               call repmi ( phrase, '#', frames(i), phrase )
            end if

            call repmi ( phrase, '#', endps(i),  phrase )
            call repmc ( phrase, '@', bname(i),  phrase )

            call suffix ( phrase, 1, errmsg )

!
!           The error messages below are appended only if they're not
!           redundant.
!
            if (  ( i .eq. 1 )  .or.  ( endps(2) .ne. endps(1) )  ) then
!
!              For each endpoint frame, if that frame is of CK type,
!              indicate the instrument ID for which CK data are needed.
!
               call frinfo ( endps(i), center, class, clssid, found )

               if ( failed() ) then
                  call chkout ( 'ZZNOFCON' )
                  return
               end if



               if ( found ) then

                  if ( class .eq. ck ) then

                     phrase = 'The latter is a CK frame; a CK file ' &
                     //       'containing data for instrument or ' &
                     //       'structure # at the epoch shown above, ' &
                     //       'as well as a corresponding SCLK kernel, ' &
                     //       'must be loaded in order to use this ' &
                     //       'frame.'

                     call repmi ( phrase, '#', clssid, phrase )

                     call suffix ( phrase, 1, errmsg )

!
!                    Find out whether we have SCLK data for this
!                    CK ID.
!
                     call ckmeta ( clssid, 'SCLK', sclkid )

                     if (  .not. zzsclk ( clssid, sclkid )  ) then

                        scmiss = .true.

                        phrase = 'No SCLK kernel for instrument or ' &
                        //       'structure #, with corresponding ' &
                        //       'SCLK ID #, is currently loaded.'

                        call repmi ( phrase, '#', clssid, phrase )
                        call repmi ( phrase, '#', sclkid, phrase )

                        call suffix ( phrase, 1, errmsg )


                     else
!
!                       If we got here and have the SCLK data, then
!                       we don't have CK data.
!
                        ckmiss = .true.

                     end if

                  end if
!
!                 End of CK frame case.
!
               end if
!
!              End of "info found" case.
!
            end if
!
!           End of distinct frame case.
!

         else if (        ( i        .eq. 1        ) &
                    .or.  ( endps(2) .ne. endps(1) )  ) then
!
!           The error messages below are appended only if they're not
!           redundant.
!
!           This graph has length one. If the frame comprising
!           this graph is a CK frame, generate a phrase
!           indicating the needed CK data.
!
            call frinfo ( frames(i), center, class, clssid, found )

            if ( failed() ) then
               call chkout ( 'ZZNOFCON' )
               return
            end if


            if ( found ) then

               if ( class .eq. ck ) then

                  phrase = '# is a CK frame; a CK file ' &
                  //       'containing data for instrument or ' &
                  //       'structure # at the epoch shown above, ' &
                  //       'as well as a corresponding SCLK kernel, ' &
                  //       'must be loaded in order to use this ' &
                  //       'frame.'

                  if ( havnam(i) ) then
                     call repmc ( phrase, '#', name(i),   phrase )
                  else
                     call repmi ( phrase, '#', frames(i), phrase )
                  end if

                  call repmi ( phrase, '#', clssid, phrase )

                  call suffix ( phrase, 1, errmsg )

!
!                 Find out whether we have SCLK data for this
!                 CK ID.
!
                  call ckmeta ( clssid, 'SCLK', sclkid )

                  if (  .not. zzsclk ( clssid, sclkid )  ) then

                     scmiss = .true.

                     phrase = 'No SCLK kernel for instrument or ' &
                     //       'structure #, with corresponding ' &
                     //       'SCLK ID #, is currently loaded.'

                     call repmi ( phrase, '#', clssid, phrase )
                     call repmi ( phrase, '#', sclkid, phrase )

                     call suffix ( phrase, 1, errmsg )

                  else
!
!                    If we got here and have the SCLK data, then
!                    we don't have CK data.
!
                     ckmiss = .true.

                  end if

               end if
!
!              End of CK frame case.
!
            end if
!
!           End of "info found" case.
!
         end if
!
!        End of path length case.
!
      end do
!
!     End of path loop.
!

      if ( ckmiss ) then
!
!        At least one lookup failed due to missing CK data.
!
!        The informational message we include depends on whether we
!        also lack SCLK data.
!
         if ( scmiss ) then
!
!           We lack SCLK data for one frame and CK data for another.
!
            phrase = 'For a CK frame for which the corresponding SCLK ' &
            //       'kernel has been loaded, ' &
            //       'failure to find required CK data ' &
            //       'could be due to one or more CK files ' &
            //       'not having been loaded, or to the epoch ' &
            //       'shown above lying within a coverage gap or ' &
            //       'beyond the coverage bounds of the loaded CK ' &
            //       'files. It is also possible that no loaded CK ' &
            //       'file has required angular velocity data for ' &
            //       'the input epoch, even if a loaded CK ' &
            //       'does have attitude data for that epoch. ' &
            //       'You can use CKBRIEF with the -dump option ' &
            //       'to display coverage intervals of a CK file.'

         else
!
!           We have SCLK data but lack CK data.
!
            phrase = 'Failure to find required CK data ' &
            //       'could be due to one or more CK files ' &
            //       'not having been loaded, or to the epoch ' &
            //       'shown above lying within a coverage gap or ' &
            //       'beyond the coverage bounds of the loaded CK ' &
            //       'files. It is also possible that no loaded CK ' &
            //       'file has required angular velocity data for ' &
            //       'the input epoch, even if a loaded CK ' &
            //       'does have attitude data for that epoch. ' &
            //       'You can use CKBRIEF with the -dump option ' &
            //       'to display coverage intervals of a CK file.'

         end if

         call suffix ( phrase, 1, errmsg )

      end if

      call chkout ( 'ZZNOFCON' )
      return
      end
