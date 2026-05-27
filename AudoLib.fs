module Game.AudioLib
open FSound
open System
let gunshot =
    seq {
        for n in 0 .. 4000 do
            let t = float n / 44100.0

            // descending frequency
            let freq = 800.0 - (t * 4000.0)

            // crude square wave
            let wave =
                if sin(2.0 * Math.PI * freq * t) > 0.0
                then 12000.0
                else -12000.0

            // decay envelope
            let env = exp(-25.0 * t)

            yield wave * env
    }