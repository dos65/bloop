package bloop.engine

import bloop.CompilerCache
import bloop.cli.{CommonOptions, ExitStatus}
import bloop.data.{ClientInfo, LoadedProject, WorkspaceSettings}
import bloop.engine.caches.StateCache
import bloop.io.AbsolutePath
import bloop.logging.Logger
import monix.eval.Task

import scala.annotation.tailrec

case class WorkspaceState(
    main: State,
    meta: List[State],
    client: ClientInfo,
    pool: ClientPool,
    commonOptions: CommonOptions,
    logger: Logger,
    status: ExitStatus
) { self =>

  def mergeStatus(newStatus: ExitStatus): WorkspaceState =
    this.copy(status = ExitStatus.merge(status, newStatus))

  def withError(message: String, e: Throwable): WorkspaceState = {
    logger.error(message)
    logger.trace(e)
    mergeStatus(ExitStatus.UnexpectedError)
  }

  def origin: AbsolutePath = main.build.origin

  def allStates: List[State] = main :: meta

  def allLoadedProjects: List[LoadedProject] = allStates.flatMap(_.build.loadedProjects)

  def getStateByPath(p: AbsolutePath): Option[State] =
    allStates.find(st => {
      st.build.loadedProjects.exists(_.project.baseDirectory == p)
    })

  def updateStates(updated: List[State]): WorkspaceState = {
    val init = (Option.empty[State], List.empty[State])
    val (newMain, updMeta) = updated.foldLeft(init){
      case ((None, metaAcc), st) if main.build.origin == st.build.origin => (Some(st), metaAcc)
      case ((x, metaAcc), st) => (x, st :: metaAcc)
    }

    val newMeta = {
      val curr = meta.map(v => v.build.origin -> v).toMap
      val upd = updMeta.map(v => v.build.origin -> v).toMap
      (curr ++ upd).values.toList
    }

    val applyMain = newMain.fold(self)(m => self.copy(main = m))
    applyMain.copy(meta = newMeta)
  }

}

object WorkspaceState {

  def loadActiveStateFor(
    configDir: bloop.io.AbsolutePath,
    client: ClientInfo,
    pool: ClientPool,
    opts: CommonOptions,
    logger: Logger,
    clientSettings: Option[WorkspaceSettings] = None
  ): Task[WorkspaceState] = {

    val loadState = (dir: AbsolutePath) => {
      State.loadActiveStateFor(
        dir,
        client,
        pool,
        opts,
        logger,
        clientSettings
      )
    }

    val metaDirs = sbtDirectoriesRecursively(configDir.getParent, List.empty)

    val main = loadState(configDir)
    val meta = Task.gatherUnordered(metaDirs.map(loadState))

    Task.mapBoth(main, meta)((mainS, metaS) => WorkspaceState(mainS, metaS, client, pool, opts, logger, ExitStatus.Ok))
  }

  def fromCache(
    configDir: bloop.io.AbsolutePath,
    client: ClientInfo,
    pool: ClientPool,
    opts: CommonOptions,
    logger: Logger,
    clientSettings: Option[WorkspaceSettings] = None
  ): Option[WorkspaceState] = {

    val loadState = (dir: AbsolutePath) => {
      State.stateCache.getStateFor(
        dir,
        client,
        pool,
        opts,
        logger
      )
    }

    val metaDirs = sbtDirectoriesRecursively(configDir.getParent, List.empty)
    val main = loadState(configDir)
    val meta = metaDirs.map(loadState)

    (main, meta) match {
      case (None, _ ) => None
      case (Some(_), metaStates) if metaStates.exists(_.isEmpty) => None
      case (Some(main), metaStates) =>
        val st = WorkspaceState(main, metaStates.flatten, client, pool, opts, logger, ExitStatus.Ok)
        Some(st)
    }
  }

  def loadSynchronously(
    configDir: bloop.io.AbsolutePath,
    client: ClientInfo,
    pool: ClientPool,
    opts: CommonOptions,
    logger: Logger,
    clientSettings: Option[WorkspaceSettings] = None
  ): WorkspaceState = {

    val loadState = (dir: AbsolutePath) => {
      val loadedProjects = BuildLoader.loadSynchronously(dir, logger)
      val build = Build(dir, loadedProjects, clientSettings)
      State(build, client, pool, opts, logger)
    }

    val metaDirs = sbtDirectoriesRecursively(configDir.getParent, List.empty)

    val main = loadState(configDir)
    val meta = metaDirs.map(loadState)

    WorkspaceState(main, meta, client, pool, opts, logger, ExitStatus.Ok)
  }

  def loadSynchronouslyForTests(
    configDir: bloop.io.AbsolutePath,
    compilerCache: CompilerCache,
    logger: Logger,
    clientSettings: Option[WorkspaceSettings] = None
  ): WorkspaceState = {

    val loadState = (dir: AbsolutePath) => {
      val loadedProjects = BuildLoader.loadSynchronously(dir, logger)
      val build = Build(dir, loadedProjects, clientSettings)
      State.forTests(build, compilerCache, logger)
    }

    val metaDirs = sbtDirectoriesRecursively(configDir.getParent, List.empty)

    val main = loadState(configDir)
    val meta = metaDirs.map(loadState)

    val opts = CommonOptions.default
    val clientInfo = ClientInfo.CliClientInfo(useStableCliDirs = true, () => true)

    WorkspaceState(main, meta, clientInfo, NoPool, opts, logger, ExitStatus.Ok)
  }

  @tailrec
  private def sbtDirectoriesRecursively(p: AbsolutePath, out: List[AbsolutePath]): List[AbsolutePath] = {
    val projectDir = p.resolve("project")
    val sbtDir = projectDir.resolve(".bloop")
    if (sbtDir.exists && sbtDir.isDirectory)
      sbtDirectoriesRecursively(projectDir, sbtDir :: out)
    else
      out
  }
}