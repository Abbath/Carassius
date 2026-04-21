package main

import "core:fmt"
import "core:mem"

debug_stuff :: proc() {
  track: mem.Tracking_Allocator
  mem.tracking_allocator_init(&track, context.allocator)
  context.allocator = mem.tracking_allocator(&track)

  defer {
    if len(track.allocation_map) > 0 {
      fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
      for _, entry in track.allocation_map {
        fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
      }
    }
    mem.tracking_allocator_destroy(&track)
  }
}

