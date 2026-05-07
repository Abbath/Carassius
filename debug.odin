package main

import "core:fmt"
import "core:mem"

track: mem.Tracking_Allocator

debug_stuff_init :: proc() -> mem.Allocator {
  mem.tracking_allocator_init(&track, context.allocator)
  return mem.tracking_allocator(&track)
}

debug_stuff_defer :: proc() {
  if len(track.allocation_map) > 0 {
    fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
    for _, entry in track.allocation_map {
      fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
    }
  }
  mem.tracking_allocator_destroy(&track)
}

