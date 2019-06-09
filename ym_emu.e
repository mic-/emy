-- YM emulator in Euphoria
-- Mic, 2019

without type_check

include std/convert.e
include std/math.e
include std/utils.e

export constant SAMPLE_RATE = 44100

constant YM_FRAME_SAMPLES = floor(SAMPLE_RATE /50)

constant MAX_DIGIDRUMS = 256

constant TIMER_MULT = 4

-- YM registers
enum
    YM_TONEGENL_A,
    YM_TONEGENH_A,
    YM_TONEGENL_B,
    YM_TONEGENH_B,
    YM_TONEGENL_C,
    YM_TONEGENH_C,
    YM_NOISE_CTRL,
    YM_MIXER,
    YM_LEVEL_A,
    YM_LEVEL_B,
    YM_LEVEL_C,
    YM_ENVE_FREQL,
    YM_ENVE_FREQH,
    YM_ENVE_SHAPE

-- YM5/6 software effects
enum
    SFX_SID_VOICE = 0,
    SFX_DIGI_DRUM,
    SFX_SINUS_SID,
    SFX_SYNC_BUZZ,
    SFX_NONE = 64

-- Digidrum formats
enum
    DIGI_DRUM_U4 = 0,
    DIGI_DRUM_U8,
    DIGI_DRUM_S8

enum
    EFFECT_TYPE,
    EFFECT_SAMPLENUM,
    EFFECT_AMPLITUDE = 2,
    EFFECT_SAMPLEPOS,
    EFFECT_VALUE = 3,
    EFFECT_TIMER_COUNT,
    EFFECT_TIMER_PERIOD,
    EFFECT_CHANNEL,
    EFFECT_CHANNEL_STICKY

constant YM_VOL_TB = {
    -- for (i : 0..15) YM_VOL_TB[i] = floor(power(10, (i-15)/6.67)*82)
  42,59,84,118,167,237,335,473,668,944,1333,1883,2661,3758,5309,7500
}

constant YM2149_VOL_TB = {
    35,42,50,59,70,84,99,118,141,167,199,236,281,334,397,472,562,668,793,943,1121,1333,1584,1883,2238,2660,3162,3758,4466,5309,6310,7500
}

constant TIMER_PRESCALER_TB = {
    0,
    4   * TIMER_MULT,
    10  * TIMER_MULT,
    16  * TIMER_MULT,
    50  * TIMER_MULT,
    64  * TIMER_MULT,
    100 * TIMER_MULT,
    200 * TIMER_MULT
}

constant YM_DIGIDRUM_8TO4_TB = {
    0,0,1,1,3,4,4,5,5,5,6,6,6,6,7,7,
    7,7,7,7,8,8,8,8,8,8,8,8,9,9,9,9,
    9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10,
    10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,
    11,11,11,11,11,11,11,11,11,11,11,11,11,11,12,12,
    12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,
    12,12,12,12,12,12,12,12,12,12,12,12,12,13,13,13,
    13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
    13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
    13,13,13,13,13,13,13,13,13,13,14,14,14,14,14,14,
    14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,
    14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,
    14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,
    14,14,14,14,14,14,14,14,14,14,15,15,15,15,15,15,
    15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
    15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
}

atom ymData
atom ymRegStream
sequence ymEnvTable
sequence ymRegs = repeat(0, 16)
integer frame, numFrames, loopFrame, loopOffs, dataOffs
sequence effectOne, effectTwo
integer frameCount

sequence digiDrumPtr
sequence digiDrumLen
sequence digiDrumOverride
integer digiDrumFormat
integer numDigiDrums

integer posA, posB, posC
integer posE, posN
integer speedTN, speedE, speedT1
integer periodA, periodB, periodC, periodE, periodN
integer phaseA, phaseB, phaseC
integer attackE, altE, holdE, haltE
integer stepE, maxStepE
integer outE, outN
integer lfsr
integer volA, volB, volC
integer levelA, levelB, levelC

integer modeA, modeB, modeC
integer toneA, toneB, toneC
integer noiseA, noiseB, noiseC

export sequence ymSongTitle, ymSongAuthor, ymSongComment

-- Version of peek2u for when the source data big endian
function peek2ube(atom addr)
    return bytes_to_int(reverse(peek({addr, 2})))
end function


-- Version of peek4u for when the source data big endian
function peek4ube(atom addr)
    return bytes_to_int(reverse(peek({addr, 4})))
end function


-- Return the n:th bit of val (0 or 1).
-- Specifying a mask allows more than one bit to be obtained.
-- For example, a mask of 7 would return the n:th, n+1:th and n+2:th bits.
function nth_bit(integer val, integer n, integer mask = 1)
    return and_bits(math:shift_bits(val, n), mask)
end function


export procedure ym_emu_init(atom data)
    integer i
    integer sampleBytes

    ymData = data

    numDigiDrums = peek2ube(ymData + 0x14)
    
    digiDrumFormat = DIGI_DRUM_U8
    if and_bits(peek(ymData + 0x13), 0x04) then
        digiDrumFormat = DIGI_DRUM_U4
    elsif and_bits(peek(ymData + 0x13), 0x02) then
        digiDrumFormat = DIGI_DRUM_S8
    end if
    
    ymRegStream = ymData + 0x22

    i = 0
    digiDrumPtr = {}
    digiDrumLen = {}
    while numDigiDrums do
        sampleBytes = peek4ube(ymRegStream)
        ymRegStream += 4
        if i < MAX_DIGIDRUMS then
            digiDrumPtr &= ymRegStream
            digiDrumLen &= sampleBytes
            i += 1
        end if
        ymRegStream += sampleBytes
        numDigiDrums -= 1
    end while

    ymSongTitle = peek_string(ymRegStream)
    ymRegStream += length(ymSongTitle) + 1
    ymSongAuthor = peek_string(ymRegStream)
    ymRegStream += length(ymSongAuthor) + 1
    ymSongComment = peek_string(ymRegStream)
    ymRegStream += length(ymSongComment) + 1

    speedTN = floor(64000000 / SAMPLE_RATE) * 128
    speedE = speedTN
    maxStepE = 31
    speedT1 = floor((2457600 * TIMER_MULT) / SAMPLE_RATE)
    if remainder(2457600 * TIMER_MULT, SAMPLE_RATE) > floor(SAMPLE_RATE / 2) then speedT1 += 1 end if

    ymEnvTable = YM2149_VOL_TB

    if peek(ymData + 0x17) = 0x1B then
        -- This is probably a ZX Spectrum tune with an AY clock of 1773400 Hz
        -- (0x001B0F58 big-endian). Adjust the emulation speed accordingly.
        speedTN = floor(56748800 / SAMPLE_RATE) * 128
        speedE = floor(speedTN / 2)
        maxStepE = 15
        ymEnvTable = YM_VOL_TB
    end if

    frame = 0
    lfsr = 0x10000
    
    posA = 0
    posB = 0
    posC = 0
    posE = 0
    posN = 0
    
    phaseA = 0
    phaseB = 0
    phaseC = 0

    numFrames = peek2ube(ymData + 0x0E)

    loopOffs = peek2ube(ymData + 0x1C)
    loopFrame = peek2ube(ymData + 0x1E)

    effectOne = {SFX_NONE, 0, 0, 0, 0, 0, 0}
    effectTwo = {SFX_NONE, 0, 0, 0, 0, 0, 0}

    digiDrumOverride = {0, 0, 0}

    dataOffs = frame
    for r = 1 to 16 do
        ymRegs[r] = peek(ymRegStream + dataOffs)
        dataOffs += numFrames
    end for

    holdE = 0
    altE = 0
    attackE = 0

    toneA = 1
    toneB = 1
    toneC = 1
    
    noiseA = 1
    noiseB = 1
    noiseC = 1
    outN = 0
    
    modeA = 0
    modeB = 0
    modeC = 0
    
    frameCount = YM_FRAME_SAMPLES
end procedure


procedure set_enve_shape(integer enveShape)
    if and_bits(enveShape, 4) then
        attackE = maxStepE
    else
        attackE = 0
    end if

    if and_bits(enveShape, 8) then
        holdE = utils:iif(and_bits(enveShape, 1), maxStepE, 0)
        altE = utils:iif(and_bits(enveShape, 2), maxStepE, 0)
    else
        holdE = maxStepE
        altE = attackE
    end if

    stepE = maxStepE
    outE = ymEnvTable[xor_bits(attackE, stepE) + 1]
end procedure


procedure set_level(integer chn, integer level)
    switch chn do
        case 0 then
            levelA = and_bits(level, 0x0F)
            modeA = and_bits(level, 0x10)
            volA = YM_VOL_TB[levelA + 1]

        case 1 then
            levelB = and_bits(level, 0x0F)
            modeB = and_bits(level, 0x10)
            volB = YM_VOL_TB[levelB + 1]

        case 2 then
            levelC = and_bits(level, 0x0F)
            modeC = and_bits(level, 0x10)
            volC = YM_VOL_TB[levelC + 1]
    end switch

    haltE = xor_bits(math:or_all({modeA, modeB, modeC}), 0x10)
end procedure


procedure calc_tone_noise_masks(sequence effect)
    toneA   = nth_bit(ymRegs[YM_MIXER], 0)
    toneB   = nth_bit(ymRegs[YM_MIXER], 1)
    toneC   = nth_bit(ymRegs[YM_MIXER], 2)

    noiseA  = nth_bit(ymRegs[YM_MIXER], 3)
    noiseB  = nth_bit(ymRegs[YM_MIXER], 4)
    noiseC  = nth_bit(ymRegs[YM_MIXER], 5)

    if (effect[EFFECT_TYPE] = SFX_DIGI_DRUM) and effect[EFFECT_TIMER_PERIOD] then
        switch effect[EFFECT_CHANNEL] do
            case 1 then
                toneA = 1
                noiseA = 1
            case 2 then
                toneB = 1
                noiseB = 1
            case 3 then
                toneC = 1
                noiseC = 1
        end switch
    end if
end procedure


function step_effect(sequence effect)
    effect[EFFECT_TIMER_COUNT] = and_bits(effect[EFFECT_TIMER_COUNT] + speedT1, 0x3FFFFFFF)
    if effect[EFFECT_TIMER_COUNT] >= effect[EFFECT_TIMER_PERIOD] then
        effect[EFFECT_TIMER_COUNT] -= effect[EFFECT_TIMER_PERIOD]
        
        switch effect[EFFECT_TYPE] do
            case SFX_SID_VOICE then
                effect[EFFECT_VALUE] = xor_bits(effect[EFFECT_VALUE], effect[EFFECT_AMPLITUDE])
                set_level(effect[EFFECT_CHANNEL] - 1, effect[EFFECT_VALUE])

            case SFX_DIGI_DRUM then
                if effect[EFFECT_SAMPLEPOS] < digiDrumLen[effect[EFFECT_SAMPLENUM] + 1] then
                    integer sampleByte = peek(digiDrumPtr[effect[EFFECT_SAMPLENUM] + 1] + effect[EFFECT_SAMPLEPOS])
                    if digiDrumFormat = DIGI_DRUM_U4 then
                        set_level(effect[EFFECT_CHANNEL] - 1, and_bits(sampleByte, 0x0F))
                    else
                        set_level(effect[EFFECT_CHANNEL] - 1, YM_DIGIDRUM_8TO4_TB[sampleByte + 1])
                    end if
                    effect[EFFECT_SAMPLEPOS] += 1
                else
                    -- The end of this digidrum sample has been reached; stop the effect
                    effect[EFFECT_CHANNEL_STICKY] = 0
                    effect[EFFECT_CHANNEL] = 0
                    effect[EFFECT_TIMER_PERIOD] = 0
                    effect[EFFECT_TYPE] = SFX_NONE
                    calc_tone_noise_masks(effect)
                end if
            case SFX_SYNC_BUZZ then
                set_enve_shape(effect[EFFECT_AMPLITUDE])
        end switch
    end if

    return effect
end function


function start_effect(sequence effect, integer effectNum)
    if effect[EFFECT_CHANNEL] then
        switch effect[EFFECT_TYPE] do
            case SFX_SID_VOICE, SFX_SYNC_BUZZ then
                integer amplitude = ymRegs[YM_LEVEL_A + effect[EFFECT_CHANNEL] - 1]
                amplitude = and_bits(amplitude, 0x0F)
                if amplitude != effect[EFFECT_AMPLITUDE] then
                    effect[EFFECT_AMPLITUDE] = amplitude
                    effect[EFFECT_VALUE] = amplitude
                end if

            case SFX_DIGI_DRUM then
                effect[EFFECT_SAMPLENUM] = ymRegs[YM_LEVEL_A + effect[EFFECT_CHANNEL] - 1]
                effect[EFFECT_SAMPLEPOS] = 0
        end switch
        integer prescalerSelection = floor(ymRegs[6 + (effectNum * 2) + 1] / 32)
        effect[EFFECT_TIMER_PERIOD] = TIMER_PRESCALER_TB[prescalerSelection + 1] * ymRegs[14 + effectNum + 1]
    else
        if effect[EFFECT_TYPE] = SFX_DIGI_DRUM then
            -- Don't stop digidrum effects until the end of the sample has been reached
            effect[EFFECT_CHANNEL] = effect[EFFECT_CHANNEL_STICKY]
        else
            effect[EFFECT_CHANNEL_STICKY] = 0
            effect[EFFECT_TIMER_PERIOD] = 0
            effect[EFFECT_TYPE] = SFX_NONE
        end if
    end if

    calc_tone_noise_masks(effect)

    return effect
end function


procedure ym_emu_seek(integer seekTimeSeconds)
    frame = seekTimeSeconds * 50
    if frame >= numFrames then frame = 0 end if
    dataOffs = frame

    for r = 1 to 16 do
        ymRegs[r] = peek(ymRegStream + dataOffs)
        dataOffs += numFrames
    end for
end procedure


export procedure ym_emu_run(integer numSamples, atom buffer)
    integer outA, outB, outC

    for i = 1 to numSamples do
        if frameCount = YM_FRAME_SAMPLES then
            if ymRegs[YM_ENVE_SHAPE] != 0xFF then
                set_enve_shape(ymRegs[YM_ENVE_SHAPE])
            end if

            modeA   = and_bits(ymRegs[YM_LEVEL_A], 0x10)
            modeB   = and_bits(ymRegs[YM_LEVEL_B], 0x10)
            modeC   = and_bits(ymRegs[YM_LEVEL_C], 0x10)

            toneA   = nth_bit(ymRegs[YM_MIXER], 0)
            toneB   = nth_bit(ymRegs[YM_MIXER], 1)
            toneC   = nth_bit(ymRegs[YM_MIXER], 2)

            noiseA  = nth_bit(ymRegs[YM_MIXER], 3)
            noiseB  = nth_bit(ymRegs[YM_MIXER], 4)
            noiseC  = nth_bit(ymRegs[YM_MIXER], 5)

            set_level(0, ymRegs[YM_LEVEL_A])
            set_level(1, ymRegs[YM_LEVEL_B])
            set_level(2, ymRegs[YM_LEVEL_C])

            periodA = ymRegs[YM_TONEGENL_A]
            periodA = or_bits(periodA, and_bits(ymRegs[YM_TONEGENH_A], 0x0F) * 0x100)
            periodA *= 0x8000

            periodB = ymRegs[YM_TONEGENL_B]
            periodB = or_bits(periodB, and_bits(ymRegs[YM_TONEGENH_B], 0x0F) * 0x100)
            periodB *= 0x8000

            periodC = ymRegs[YM_TONEGENL_C]
            periodC = or_bits(periodC, and_bits(ymRegs[YM_TONEGENH_C], 0x0F) * 0x100)
            periodC *= 0x8000

            periodN = and_bits(ymRegs[YM_NOISE_CTRL], 0x1F) * 0x8000

            periodE = ymRegs[YM_ENVE_FREQL]
            periodE = or_bits(periodE, ymRegs[YM_ENVE_FREQH] * 0x100)
            periodE *= 0x8000

            haltE = xor_bits(math:or_all({modeA, modeB, modeC}), 0x10)

            if peek(ymData + 2) = '6' then
                -- YM6 supports 4 different effects; two of which can be active at the same time.
                -- r1[7:6] selects the type of effect to use for effect 1, and r1[5:4] selects the channel to apply it on.
                -- r3[7:6] selects the type of effect to use for effect 2, and r3[5:4] selects the channel to apply it on.
                sequence effectCopy = effectOne & effectTwo
                
                effectOne[EFFECT_CHANNEL] = nth_bit(ymRegs[2], 4, 3)
                if effectOne[EFFECT_CHANNEL] then
                    effectOne[EFFECT_TYPE] = nth_bit(ymRegs[2], 6, 3)
                    effectOne[EFFECT_CHANNEL_STICKY] = effectOne[EFFECT_CHANNEL]
                end if
                
                effectTwo[EFFECT_CHANNEL] = nth_bit(ymRegs[4], 4, 3)
                if effectTwo[EFFECT_CHANNEL] then
                    effectTwo[EFFECT_TYPE] = nth_bit(ymRegs[4], 6, 3)
                    effectTwo[EFFECT_CHANNEL_STICKY] = effectTwo[EFFECT_CHANNEL]
                end if
            else
                -- YM5 and below only support SID-Voice and Digidrum.
                -- r1,r6,r14 controls the SID-voice effect, and r3,r8,r15 controls the Digidrum effect
                effectOne[EFFECT_CHANNEL] = nth_bit(ymRegs[2], 4, 3)
                if effectOne[EFFECT_CHANNEL] then
                    effectOne[EFFECT_TYPE] = SFX_SID_VOICE
                    effectOne[EFFECT_CHANNEL_STICKY] = effectOne[EFFECT_CHANNEL]
                end if

                effectTwo[EFFECT_CHANNEL] = nth_bit(ymRegs[4], 4, 3)
                if effectTwo[EFFECT_CHANNEL] then
                    if effectTwo[EFFECT_CHANNEL] != effectTwo[EFFECT_CHANNEL_STICKY] and effectTwo[EFFECT_TYPE] = SFX_DIGI_DRUM then
                        digiDrumOverride[effectTwo[EFFECT_CHANNEL_STICKY]] = 0
                        calc_tone_noise_masks(effectTwo)
                    end if
                    effectTwo[EFFECT_TYPE] = SFX_DIGI_DRUM
                    effectTwo[EFFECT_CHANNEL_STICKY] = effectTwo[EFFECT_CHANNEL]
                end if
            end if

            effectOne = start_effect(effectOne, 0)
            effectTwo = start_effect(effectTwo, 1)
        end if

        posA = and_bits(posA + speedTN, 0x3FFFFFFF)
        posB = and_bits(posB + speedTN, 0x3FFFFFFF)
        posC = and_bits(posC + speedTN, 0x3FFFFFFF)

        if posA >= periodA then
            posA -= periodA
            phaseA = xor_bits(phaseA, 1)
        end if

        if posB >= periodB then
            posB -= periodB
            phaseB = xor_bits(phaseB, 1)
        end if

        if posC >= periodC then
            posC -= periodC
            phaseC = xor_bits(phaseC, 1)
        end if

        if not haltE then
            posE = and_bits(posE + speedE, 0x3FFFFFFF)
            if posE >= periodE then
                posE -= periodE
                stepE -= 1
                if stepE < 0 then
                    attackE = xor_bits(attackE, altE)
                    stepE = and_bits(stepE, xor_bits(holdE, maxStepE))
                    haltE = holdE
                end if
                outE = ymEnvTable[xor_bits(stepE, attackE) + 1]
            end if
        end if

        posN = and_bits(posN + speedTN, 0x3FFFFFFF)
        if posN >= periodN then
            posN -= periodN
            outN = and_bits(lfsr, 1)
            lfsr = or_bits(floor(lfsr / 2), xor_bits(outN, nth_bit(lfsr, 3)) * 0x10000)
        end if

        if effectOne[EFFECT_TIMER_PERIOD] then
            effectOne = step_effect(effectOne)
        end if
        if effectTwo[EFFECT_TIMER_PERIOD] then
            effectTwo = step_effect(effectTwo)
        end if
        
        outA = -and_bits(or_bits(phaseA, toneA), or_bits(outN, noiseA))
        outB = -and_bits(or_bits(phaseB, toneB), or_bits(outN, noiseB))
        outC = -and_bits(or_bits(phaseC, toneC), or_bits(outN, noiseC))

        if modeA then
            outA *= outE
        else
            outA *= volA
        end if

        if modeB then
            outB *= outE
        else
            outB *= volB
        end if

        if modeC then
            outC *= outE
        else
            outC *= volC
        end if
        
        poke2(buffer, outA + outB + outC + 9000)
        buffer += 2

        if not frameCount then
            frameCount = YM_FRAME_SAMPLES
            frame += 1
            if frame >= numFrames then
                frame = 0
            end if

            dataOffs = frame
            for r = 1 to 16 do
                ymRegs[r] = peek(ymRegStream + dataOffs)
                dataOffs += numFrames
            end for
        else
            frameCount -= 1
        end if
    end for
end procedure
