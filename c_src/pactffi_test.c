/* niftest.c */
#include <pact.h>
#include <string.h>
#include <stdio.h>


int test_pactffi() {
    printf("Verification Start--------------------\n");

    char *name = "animal_service";
    char *scheme = "http";
    char *host = "localhost";
    int port = 8080;
    char *path = "/";
    char *version = "default";
    char *branch = "develop";
    char *broker_url = "http://localhost:9292/";
    // OSS pact broker docker image: pactfoundation/pact-broker
    char *broker_username = "pact_workshop";
    char *broker_password = "pact_workshop";
    int enable_pending = 1;
    char *consumer_version_selectors = "{}";
    int consumer_version_selectors_len = 0;
    char *protocol = "http";

    
    char *state_path = "http://localhost:8080/pactStateChange";

    struct VerifierHandle *verifierhandle;
    verifierhandle = pactffi_verifier_new_for_application(name, version);
    pactffi_verifier_set_provider_info(verifierhandle, name, scheme, host, port, path);
    pactffi_verifier_add_provider_transport(verifierhandle, protocol, port, path, scheme);

    if (state_path[0] != '\0')
    {
        pactffi_verifier_set_provider_state(verifierhandle, state_path, 0, 1);
    }
    pactffi_verifier_set_verification_options(verifierhandle, 0, 5000),
    pactffi_verifier_set_publish_options(verifierhandle, version, NULL, NULL, -1, branch);
    pactffi_verifier_broker_source_with_selectors(verifierhandle, broker_url, broker_username, broker_password, NULL, enable_pending, NULL, NULL, -1, branch, consumer_version_selectors, consumer_version_selectors_len, NULL, -1);
    setenv("PACT_DO_NOT_TRACK", "true", 1);
    int output = pactffi_verifier_execute(verifierhandle);
    pactffi_verifier_shutdown(verifierhandle);

    printf("Verification End------------------------\n");


    return output;
}

int main() {
    // Function call
    pactffi_logger_init();
    pactffi_logger_attach_sink("stdout", 5);
    pactffi_logger_apply();
    int output = test_pactffi();

    printf("Output is ------ %d\n", output);
    return 0;
}