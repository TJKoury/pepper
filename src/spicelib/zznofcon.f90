!$Procedure  ZZNOFCON ( Create frame connection long error message )

      SUBROUTINE ZZNOFCON ( ET, FRAME1, ENDP1, FRAME2, ENDP2, ERRMSG )

      IMPLICIT NONE

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Create an informative long error message for cases where the
!     frame system signals a SPICE(NOFRAMECONNECT) error.
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

      INCLUDE 'frmtyp.inc'

      DOUBLE PRECISION      ET
      INTEGER               FRAME1
      INTEGER               ENDP1
      INTEGER               FRAME2
      INTEGER               ENDP2
      CHARACTER*(*)         ERRMSG

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
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               ZZSCLK

!
!     Local parameters
!
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 35 )

      INTEGER               PHRSLN
      PARAMETER           ( PHRSLN = 600 )

!
!     Local variables
!
      CHARACTER*(FRNMLN)    BNAME ( 2 )
      CHARACTER*(FRNMLN)    NAME  ( 2 )
      CHARACTER*(PHRSLN)    PHRASE
      CHARACTER*(TIMLEN)    TIMSTR

      INTEGER               ENDPS  ( 2 )
      INTEGER               CENTER
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               FRAMES ( 2 )
      INTEGER               I
      INTEGER               SCLKID

      LOGICAL               CKMISS
      LOGICAL               FOUND
      LOGICAL               HAVNAM ( 2 )
      LOGICAL               SCMISS

!
!     Because this routine might cause a SPICE error to be
!     signaled, we have to check in.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZNOFCON' )

!
!     Capture input IDs in arrays.
!
      FRAMES(1) = FRAME1
      FRAMES(2) = FRAME2
      ENDPS(1)  = ENDP1
      ENDPS(2)  = ENDP2

!
!     The flags CKMISS and SCMISS are used, respectively, to
!     record whether any CK lookup failed due to missing CK
!     data or missing SCLK data. Each of these flags is turned
!     on if at least one lookup failed due to the indicated
!     cause.
!
      CKMISS = .FALSE.
      SCMISS = .FALSE.

!
!     Get a string representation of the transformation epoch.
!
      CALL ETCAL ( ET, TIMSTR )

!
!     Get the names of the participating frames, if available.
!
      CALL FRMNAM ( FRAMES(1),   NAME(1) )
      CALL FRMNAM ( FRAMES(2),   NAME(2) )
      CALL FRMNAM ( ENDPS(1),    BNAME(1) )
      CALL FRMNAM ( ENDPS(2),    BNAME(2) )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZNOFCON' )
         RETURN
      END IF

      DO I = 1, 2

         IF ( NAME(I) .EQ. ' ' ) THEN

            NAME(I)   = 'Name not available'
            HAVNAM(I) = .FALSE.
         ELSE
            HAVNAM(I) = .TRUE.
         END IF

         IF ( BNAME(I) .EQ. ' ' ) THEN
            BNAME(I) = 'Name not available'
         END IF

      END DO

      ERRMSG = 'At epoch # TDB (# TDB), there is ' &
      //       'insufficient information available to ' &
      //       'transform from reference frame # (@) to ' &
      //       'reference frame # (@).'


      CALL REPMF ( ERRMSG, '#', ET,     14, 'E', ERRMSG )
      CALL REPMC ( ERRMSG, '#', TIMSTR,          ERRMSG )

      DO I = 1, 2

         CALL REPMI ( ERRMSG, '#', FRAMES(I), ERRMSG )
         CALL REPMC ( ERRMSG, '@', NAME(I),   ERRMSG )

      END DO

!
!     For any frame graph longer than a single point, tell the user
!     the endpoint of the frame connection graph originating
!     at that frame.
!
      DO I = 1, 2

         IF ( FRAMES(I) .NE. ENDPS(I)  ) THEN

            PHRASE = 'Frame # could be transformed to frame # (@).'

            IF ( HAVNAM(I) ) THEN
               CALL REPMC ( PHRASE, '#', NAME(I),   PHRASE )
            ELSE
               CALL REPMI ( PHRASE, '#', FRAMES(I), PHRASE )
            END IF

            CALL REPMI ( PHRASE, '#', ENDPS(I),  PHRASE )
            CALL REPMC ( PHRASE, '@', BNAME(I),  PHRASE )

            CALL SUFFIX ( PHRASE, 1, ERRMSG )

!
!           The error messages below are appended only if they're not
!           redundant.
!
            IF (  ( I .EQ. 1 )  .OR.  ( ENDPS(2) .NE. ENDPS(1) )  ) THEN
!
!              For each endpoint frame, if that frame is of CK type,
!              indicate the instrument ID for which CK data are needed.
!
               CALL FRINFO ( ENDPS(I), CENTER, CLASS, CLSSID, FOUND )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'ZZNOFCON' )
                  RETURN
               END IF



               IF ( FOUND ) THEN

                  IF ( CLASS .EQ. CK ) THEN

                     PHRASE = 'The latter is a CK frame; a CK file ' &
                     //       'containing data for instrument or ' &
                     //       'structure # at the epoch shown above, ' &
                     //       'as well as a corresponding SCLK kernel, ' &
                     //       'must be loaded in order to use this ' &
                     //       'frame.'

                     CALL REPMI ( PHRASE, '#', CLSSID, PHRASE )

                     CALL SUFFIX ( PHRASE, 1, ERRMSG )

!
!                    Find out whether we have SCLK data for this
!                    CK ID.
!
                     CALL CKMETA ( CLSSID, 'SCLK', SCLKID )

                     IF (  .NOT. ZZSCLK ( CLSSID, SCLKID )  ) THEN

                        SCMISS = .TRUE.

                        PHRASE = 'No SCLK kernel for instrument or ' &
                        //       'structure #, with corresponding ' &
                        //       'SCLK ID #, is currently loaded.'

                        CALL REPMI ( PHRASE, '#', CLSSID, PHRASE )
                        CALL REPMI ( PHRASE, '#', SCLKID, PHRASE )

                        CALL SUFFIX ( PHRASE, 1, ERRMSG )


                     ELSE
!
!                       If we got here and have the SCLK data, then
!                       we don't have CK data.
!
                        CKMISS = .TRUE.

                     END IF

                  END IF
!
!                 End of CK frame case.
!
               END IF
!
!              End of "info found" case.
!
            END IF
!
!           End of distinct frame case.
!

         ELSE IF (        ( I        .EQ. 1        ) &
                    .OR.  ( ENDPS(2) .NE. ENDPS(1) )  ) THEN
!
!           The error messages below are appended only if they're not
!           redundant.
!
!           This graph has length one. If the frame comprising
!           this graph is a CK frame, generate a phrase
!           indicating the needed CK data.
!
            CALL FRINFO ( FRAMES(I), CENTER, CLASS, CLSSID, FOUND )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZNOFCON' )
               RETURN
            END IF


            IF ( FOUND ) THEN

               IF ( CLASS .EQ. CK ) THEN

                  PHRASE = '# is a CK frame; a CK file ' &
                  //       'containing data for instrument or ' &
                  //       'structure # at the epoch shown above, ' &
                  //       'as well as a corresponding SCLK kernel, ' &
                  //       'must be loaded in order to use this ' &
                  //       'frame.'

                  IF ( HAVNAM(I) ) THEN
                     CALL REPMC ( PHRASE, '#', NAME(I),   PHRASE )
                  ELSE
                     CALL REPMI ( PHRASE, '#', FRAMES(I), PHRASE )
                  END IF

                  CALL REPMI ( PHRASE, '#', CLSSID, PHRASE )

                  CALL SUFFIX ( PHRASE, 1, ERRMSG )

!
!                 Find out whether we have SCLK data for this
!                 CK ID.
!
                  CALL CKMETA ( CLSSID, 'SCLK', SCLKID )

                  IF (  .NOT. ZZSCLK ( CLSSID, SCLKID )  ) THEN

                     SCMISS = .TRUE.

                     PHRASE = 'No SCLK kernel for instrument or ' &
                     //       'structure #, with corresponding ' &
                     //       'SCLK ID #, is currently loaded.'

                     CALL REPMI ( PHRASE, '#', CLSSID, PHRASE )
                     CALL REPMI ( PHRASE, '#', SCLKID, PHRASE )

                     CALL SUFFIX ( PHRASE, 1, ERRMSG )

                  ELSE
!
!                    If we got here and have the SCLK data, then
!                    we don't have CK data.
!
                     CKMISS = .TRUE.

                  END IF

               END IF
!
!              End of CK frame case.
!
            END IF
!
!           End of "info found" case.
!
         END IF
!
!        End of path length case.
!
      END DO
!
!     End of path loop.
!

      IF ( CKMISS ) THEN
!
!        At least one lookup failed due to missing CK data.
!
!        The informational message we include depends on whether we
!        also lack SCLK data.
!
         IF ( SCMISS ) THEN
!
!           We lack SCLK data for one frame and CK data for another.
!
            PHRASE = 'For a CK frame for which the corresponding SCLK ' &
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

         ELSE
!
!           We have SCLK data but lack CK data.
!
            PHRASE = 'Failure to find required CK data ' &
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

         END IF

         CALL SUFFIX ( PHRASE, 1, ERRMSG )

      END IF

      CALL CHKOUT ( 'ZZNOFCON' )
      RETURN
      END
