#!/bin/bash

cert_path="$1"

validate_cert() {
    input_path="$1"
    cert_info=$(openssl x509 -in "$input_path" -noout -text 2>/dev/null)
    if echo "$cert_info" | grep -q "Subject Alternative Name"; then
        return 0
    else
        return 1
    fi
}

is_root_ca() {
    input_path="$1"

    #Check the certificate is RootCA is not
    is_self_signed=$(openssl verify -CAfile "$cert" "$cert" 2>&1)

    #enable case insensitive match for reg-ex
    shopt -s nocasematch
    if [[ ! -z $(cat "$cert") ]] && [[ $is_self_signed == *error* ]]; then
            #disable case insensitive match for reg-ex
            shopt -u nocasematch
            return 1
    else
            #disable case insensitive match for reg-ex
            shopt -u nocasematch
            return 0
    fi

}

# If the certificate path provide validate only that certificate
if [ "$cert_path" ]; then
    # Check for the certificate file exists
    if [ ! -f "$cert_path" ]; then
            echo "Error: Certificate file not found."
            exit 1
    fi

    if validate_cert "$cert_path"; then
        echo "Pass: your certificate is a valid SAN certificate"
    else
        echo "Fail: your certificate is not a valid SAN certificate"
    fi
    exit 1
fi


# Get all the internal certificates with extension(.crt), gets the RootCA and public certificates
certs=$(find /hab/svc -type f -name "*.crt")
invalid_certs=()

for cert in $certs; do
        if ! is_root_ca "$cert" && ! validate_cert "$cert"; then
                invalid_certs+=("$cert")
        fi
done

# Print the list of invalid certs
echo "Invalid certificates:"
for cert in "${invalid_certs[@]}"; do
    echo "$cert"
done