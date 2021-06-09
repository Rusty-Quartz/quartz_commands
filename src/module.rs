pub trait CommandModule<C> {
    fn dispatch(&self, command: &str, context: C) -> Result<(), String>;

    fn get_suggestions(&self, command: &str, context: &C) -> Vec<String>;
}
