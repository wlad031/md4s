package dev.vgerasimov.md4s
package common
package models

import java.nio.file.Path

trait Document

trait DocumentWithPath[D <: Document](document: D, path: Path)
