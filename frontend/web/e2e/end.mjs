// The end of an e2e script: stops the backend and closes the browser, with
// a deadline. Now and then a browser's close never returns (on a busy machine,
// helpers of it holding on), which would keep the run waiting for good: the
// script then exits anyway, failed if it had failed, once its output is all
// out.
export const end = async (...steps) => {
  const deadline = setTimeout(() => {
    console.error("e2e: the browser or the backend did not go away in 20s; exiting");
    process.exit(process.exitCode ?? 0);
  }, 20000);
  deadline.unref();
  for (const step of steps) await step();
};
