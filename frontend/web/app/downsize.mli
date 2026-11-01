open! Core

(** Re-encodes an image attached to the next prompt smaller on a canvas
    ([Command.Downsize_image]), reporting [Action.Downsized] or
    [Action.Downsize_failed]: at the target's size, as PNG when that fits
    (a PNG or GIF: screenshots and drawings stay sharp), else JPEG at
    falling qualities, then at three quarters of the size, until it is
    under the target's bytes. *)

val install : schedule:(Prigh_web.App.Action.t -> unit) -> unit
val run : Prigh_protocol.Image.t -> Prigh_web.Image_size.Target.t -> unit
