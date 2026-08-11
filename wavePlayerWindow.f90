MODULE wavePlayerWindow

    USE, INTRINSIC :: ISO_C_BINDING
    USE WINTERACTER
    USE RESID
    USE IFWIN
    USE IFWINTY
    USE WINAPIS
    USE debugWindow
    USE engineConstants  
    USE subs
    USE KERNEL32
    use WINMM
    USE adlib
    USE TIA
    USE Waveplayer
    USE inpout

    implicit none

    PRIVATE
    PUBLIC :: soundSettings, resetSoundSettings, checkForSoundSettingUpdates, sendTheValues 

    integer(2)                              :: sfxVolume   , musicVolume   , LPTAddress   , &
                                               sfxVolumeOld, musicVolumeOld, LPTAddressOld      
    logical                                 :: OPL2LPT, OPL2LPTOld, canKill
    logical                                 :: musicPlaying     = .FALSE.,  &
                                               playingOnStartup = .FALSE.

    TYPE SoundSettingsBuffer
         logical                            :: opl2LPTSelected
         integer(2)                         :: sfxVolTrk, musicVolTrk, sfxVolFld, musicVolFld
         character(4)                       :: LPTAddr

    end TYPE

    TYPE(SoundSettingsBuffer)              :: oldSettings, newSettings

    CONTAINS

    subroutine resetSoundSettings()
            sfxVolume    = 100
            musicVolume  = 100
            LPTAddress   = Z'0378'
            OPL2LPT      = .FALSE.
    
            call moveValuesToFields(oldSettings) 
            call moveValuesToFields(newSettings) 

    end subroutine

    subroutine moveValuesToFields(group)
         type(SoundSettingsBuffer)              :: group   

         group%opl2LPTSelected    = OPL2LPT      
         group%sfxVolTrk          = sfxVolume    
         group%musicVolTrk        = musicVolume  
         group%sfxVolFld          = sfxVolume      
         group%musicVolFld        = musicVolume   
         
         write(group%LPTAddr, "(Z4.4)") LPTAddress    

    end subroutine

    subroutine moveFieldsToValues(group)
         type(SoundSettingsBuffer)              :: group   

         OPL2LPT                  = group%opl2LPTSelected
         sfxVolume                = group%sfxVolTrk
         musicVolume              = group%musicVolTrk
         sfxVolume                = group%sfxVolFld
         musicVolume              = group%musicVolFld   
         
         read(group%LPTAddr, "(Z4.4)") LPTAddress    

    end subroutine

    subroutine MoveValuesToDialog()
         character(4)             :: temp

         if (OPL2LPT) then
             call WDialogPutRadioButton(IDF_MusicRadio2)
         else
             call WDialogPutRadioButton(IDF_MusicRadio1)
         end if

         call WDialogPutInteger( IDF_SoundValue, sfxVolume   )
         call WDialogPutTrackbar(IDF_SoundTrack, sfxVolume   )
         call WDialogPutInteger( IDF_MusicValue, musicVolume )
         call WDialogPutTrackbar(IDF_MusicTrack, musicVolume )

         write(temp, "(Z4.4)") LPTAddress
         call WDialogPutString( IDF_LPTPort, temp)

    end subroutine 

    subroutine moveDialogtoFields(group)
         type(SoundSettingsBuffer) :: group   
         character(4)             :: temp
         integer(4)               :: choice

         call WDialogGetRadioButton(IDF_MusicRadio1, choice)
         group%opl2LPTSelected = (choice == 2)   

         !write(temp, "(L1)") group%opl2LPTSelected
         !call displayDebug(temp)

         call WDialogGetInteger( IDF_SoundValue, choice)
         group%sfxVolFld = choice   

         call WDialogGetTrackbar(IDF_SoundTrack, choice)
         group%sfxVolTrk = choice

         call WDialogGetInteger( IDF_MusicValue, choice)
         group%musicVolFld = choice

         call WDialogGetTrackbar(IDF_MusicTrack, choice)
         group%musicVolTrk = choice            

         call WDialogGetString( IDF_LPTPort, group%LPTAddr)
        
    end subroutine 

    subroutine sendTheValues()
         call changeTIAVolChanger(     real(sfxVolume)   / 100)
         call changeAdlibVolumeChanger(real(musicVolume) / 100)
         call setLPTAddress(LPTAddress, OPL2LPT)
    end subroutine

    function isPlaying() result(r)
        logical     :: r
        
        r = ( ((isMusicPlaying() .EQV. .TRUE.) .AND. (OPL2LPT .EQV. .FALSE.)) &
       .OR.   ((isChipPlaying()  .EQV. .TRUE.) .AND. (OPL2LPT .EQV. .TRUE. )) )
    end function 

    subroutine soundSettings()
       INTEGER                                 :: ITYPE
       TYPE(WIN_MESSAGE)                       :: MESSAGE
       !character(10)                  :: msgString
       integer                                 :: c 
       character(40)                           :: t  

       if (isPlaying() .EQV. .TRUE.) then
           playingOnStartup = .TRUE.
       else
           playingOnStartup = .FALSE.
       end if       

       musicPlaying = playingOnStartup 

       canKill = .FALSE. 
       CALL WDialogLoad(IDD_SoundSettings)
    
       sfxVolumeOld   = sfxVolume 
       musicVolumeOld = musicVolume
       LPTAddressOld  = LPTAddress
       OPL2LPTOld     = OPL2LPT        

       CALL MoveValuesToDialog()  
       CALL enableDisableFields() 

       do
          CALL WDialogSelect(IDD_SoundSettings)
          CALL WDialogShow(ITYPE=Modal)     
    
          if (WinfoDialog(CurrentDialog) == IDD_SoundSettings) then 
              SELECT CASE (WinfoDialog(ExitButton))  
                  CASE(ExitField) 
                     sfxVolume   = sfxVolumeOld 
                     musicVolume = musicVolumeOld
                     LPTAddress  = LPTAddressOld
                     OPL2LPT     = OPL2LPTOld            
                     EXIT

                  CASE(ID_SoundOK)        
                     EXIT

                  CASE(ID_SoundCancel)
                     sfxVolume   = sfxVolumeOld 
                     musicVolume = musicVolumeOld
                     LPTAddress  = LPTAddressOld
                     OPL2LPT     = OPL2LPTOld            
                     EXIT

                  CASE(ID_SoundReset)
                     call resetSoundSettings()
                     call MoveValuesToDialog()

                  CASE(ID_SoundTest)
                     call playRandomTIA(0)

                  CASE(ID_MusicTest)
                       call playPlayPlay() 
                        
                  CASE(ID_MusicTest2)
                       call playPlayPlay() 

                  CASE(ID_MusicStop)
                       call stopStopStop() 

                  CASE(ID_MusicStop2)
                       call stopStopStop() 

              END SELECT

          end if
       end do 

       canKill = .TRUE. 
       call sendTheValues()

       !open(56, FILE = 'fos666.txt', status = 'UNKNOWN', action = 'WRITE')  

       if (playingOnStartup     .EQV. .TRUE. ) then
           !write(56, '(I0)') 1 
           if ((isMusicPlaying()  .EQV. .FALSE.) .AND. (isChipPlaying() .EQV. .FALSE.)) then 
               !write(56, '(I0)') 3 
               call continueToPlayA()                
           end if 
       else 
           !write(56, '(I0)') 2 
           if ((isMusicPlaying() .EQV. .TRUE.) .OR. (isChipPlaying() .EQV. .TRUE.)) then 
               !write(56, '(I0)') 4   
               call stopMusic()
           end if            
       end if  

       close(56) 

    end subroutine

    subroutine playPlayPlay()
       if (playingOnStartup .EQV. .TRUE.) then
           call continueToPlayA()  
       else
           call playRandomAdlib()  
       end if 

       musicPlaying = .TRUE. 

    end subroutine

    subroutine stopStopStop()
       call stopMusic() 
       musicPlaying = .FALSE. 
    end subroutine    

    subroutine checkForSoundSettingUpdates()
        integer(1)              :: state

        call moveDialogtoFields(newSettings)

        if (anySFXPlaying() .EQV. .TRUE.) then
            state = DISABLED
        else
            state = ENABLED
        end if
        
        call WDialogFieldState(ID_SoundTest  , state ) 
        call WDialogFieldState(IDF_SoundValue, state ) 
        call WDialogFieldState(IDF_SoundTrack, state ) 

        if ((musicPlaying .EQV. .TRUE.) .OR. (isChipPlaying() .EQV. .TRUE.))then
            state = DISABLED
        else
            state = ENABLED
        end if

        if (OPL2LPT .EQV. .TRUE.) then
            call WDialogFieldState(ID_MusicStop  , DISABLED  ) 
            call WDialogFieldState(ID_MusicStop2 , 1 - state ) 
        else
            call WDialogFieldState(ID_MusicStop  , 1 - state ) 
            call WDialogFieldState(ID_MusicStop2 , DISABLED )         
        end if

        call WDialogFieldState(IDF_MusicRadio1,    state ) 
        call WDialogFieldState(IDF_MusicRadio2,    state )

        if (state == ENABLED) then
            call enableDisableFields()

            if (oldSettings%opl2LPTSelected .NEQV. newSettings%opl2LPTSelected) then    
                OPL2LPT = newSettings%opl2LPTSelected
            end if
    
            if (oldSettings%sfxVolTrk /= newSettings%sfxVolTrk) then
                sfxVolume = newSettings%sfxVolTrk
                newSettings%sfxVolFld = sfxVolume  
                call WDialogPutInteger(IDF_SoundValue, sfxVolume)       
            end if
    
            if (oldSettings%sfxVolFld /= newSettings%sfxVolFld) then
                sfxVolume = newSettings%sfxVolFld
                newSettings%sfxVolTrk = sfxVolume      
                call WDialogPutTrackbar(IDF_SoundTrack, sfxVolume)
            end if
    
            if (oldSettings%musicVolTrk /= newSettings%musicVolTrk) then
                musicVolume = newSettings%musicVolTrk
                newSettings%musicVolFld = musicVolume   
                call WDialogPutInteger(IDF_MusicValue, musicVolume)             
            end if
    
            if (oldSettings%musicVolFld /= newSettings%musicVolFld) then
                musicVolume = newSettings%musicVolFld
                newSettings%musicVolTrk = musicVolume    
                call WDialogPutTrackbar(IDF_MusicTrack, musicVolume)     
            end if
    
            call moveValuesToFields(oldSettings)
            call sendTheValues()
        else
            
            call WDialogFieldState(IDF_MusicValue, DISABLED ) 
            call WDialogFieldState(IDF_MusicTrack, DISABLED ) 
            call WDialogFieldState(IDF_LPTPORT   , DISABLED ) 
            call WDialogFieldState(ID_MusicTest  , DISABLED ) 
            call WDialogFieldState(ID_MusicTest2 , DISABLED ) 
        end if

        if (canKill .EQV. .TRUE.) then
            CALL WDialogUnLoad()
            canKill = .FALSE.
        end if 

    end subroutine

    subroutine enableDisableFields()
           integer(1)      :: state  

           if (newSettings%opl2LPTSelected) then            
               state = 1
           else
               state = 0 
           end if

           call WDialogFieldState(IDF_LPTPort   , state) 
           call WDialogFieldState(ID_MusicTest2 , state) 
    
           call WDialogFieldState(ID_MusicTest  , 1 - state) 
           call WDialogFieldState(IDF_MusicTrack, 1 - state) 
           call WDialogFieldState(IDF_MusicValue, 1 - state) 

    end subroutine

END MODULE wavePlayerWindow
